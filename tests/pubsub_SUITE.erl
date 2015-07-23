%%%===================================================================
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc Suite for testing pubsub features as described in XEP-0060
%%% @end
%%%===================================================================

-module(pubsub_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

%% pubsub_full_cycle_two_users
%% case where owner creates a node, publishes to it and another user gets what he published.
%% Then owner deletes the published item and deletes the node.  It's a round-trip two-users case. 




all() -> [
	  {group, pubsub_full_cycle_two_users}
%%	  {group, testgroup}
	 ].

groups() ->  [{pubsub_full_cycle_two_users, [sequence], [
							 request_to_create_node_success,
							 request_to_publish_to_node_success, 
							 request_to_subscribe_to_node_success,
							 request_to_subscribe_to_node_by_owner_success,
							 request_all_items_from_node_success,
							 users_get_notified_success,
							 request_to_retrieve_subscription_list_by_owner_success,
							 request_to_retract_item_success,
							 request_to_unsubscribe_from_node_by_owner_success,
							 request_to_delete_node_success
							]
	      },
	      {testgroup, [multiple_notifications_success]}
	     ].




suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config,{by_name, [alice, bob]}).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config,{by_name, [alice, bob]}).

init_per_testcase(request_to_subscribe_to_node_success, Config) ->
    escalus:init_per_testcase(request_to_subscribe_to_node_success, Config);

init_per_testcase(_TestName, Config) ->
    escalus:init_per_testcase(_TestName, Config).

end_per_testcase(request_to_subscribe_to_node_success, Config) ->
    escalus:end_per_testcase(request_to_subscribe_to_node_success, Config);

end_per_testcase(_TestName, Config) ->
    escalus:end_per_testcase(_TestName, Config).


%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

-define (TOPIC_SERVICE_ADDR, <<"pubsub.localhost">>).
-define (DEFAULT_TOPIC_NAME, <<"princely_musings">>).
   
%% XEP0060---8.1.1 Create a node with default configuration ---------------------------
request_to_create_node_success(Config) ->
    escalus:story(Config, [1],
		   fun(Alice) ->
			   PubSubCreate = pubsub_helper:create_specific_node_stanza(?DEFAULT_TOPIC_NAME),
			   PubSub = pubsub_helper:pubsub_stanza([PubSubCreate], ?NS_PUBSUB),
			   DestinationNode = ?TOPIC_SERVICE_ADDR,
			   Id = <<"create1">>,
			   PubSubCreateIq  =  pubsub_helper:iq_with_id(set, Id, DestinationNode, Alice,  [PubSub]),
			   ct:pal(" Request PubSubCreateIq: ~n~p~n",[exml:to_binary(PubSubCreateIq)]),
			   escalus:send(Alice, PubSubCreateIq),
			   {true, _RecvdStanza} = pubsub_tools:wait_for_stanza_and_match_result_iq(Alice, Id, DestinationNode)
			   %% example 131
		   end).

%% XEP0060---7.1.1 Request to publish to a node -----------------------------------------
%% item with ID=abc123 is being published - use this id for retract and other tests in this group
request_to_publish_to_node_success(Config) ->
     escalus:story(Config, [1],
		   fun(Alice) ->
			   {true, _RecvdStanza} = pubsub_tools:publish_sample_content(?DEFAULT_TOPIC_NAME,
										     ?TOPIC_SERVICE_ADDR,
										     <<"abc123">>, Alice, sample_one)
			   %% see example 100
		   end).


users_get_notified_success(Config) ->
 escalus:story(Config, [{alice,1},{bob,1}],
		   fun(Alice, Bob) ->
			   pubsub_tools:subscribe_by_user(Bob, ?DEFAULT_TOPIC_NAME, ?TOPIC_SERVICE_ADDR),
			   {true, _RecvdStanza} = pubsub_tools:publish_sample_content(?DEFAULT_TOPIC_NAME,
										     ?TOPIC_SERVICE_ADDR,
										     <<"xyz123">>, Alice, sample_three),

			   StanzaGot1 = escalus:wait_for_stanza(Bob),
			   io:format(" --- bob got stanza1 --- ~n~p~n", [StanzaGot1]),

			   {true, _RecvdStanza2} = pubsub_tools:publish_sample_content(?DEFAULT_TOPIC_NAME,
										     ?TOPIC_SERVICE_ADDR,
										     <<"abc123">>, Alice, sample_one),
			   StanzaGot2 = escalus:wait_for_stanza(Bob),
			   io:format(" --- bob got stanza2 --- ~n~p~n", [StanzaGot2])

		   end).

%% XEP0060---7.2.1 Request delete item from node -----------------------------------------
%% Alice as Owner and Publisher might want to delete previously published item
%% In this case it is item with ID=abc123
request_to_retract_item_success(Config) ->
    escalus:story(Config, [1],
		  fun(Alice) ->
			   {true, RecvdStanza1} = pubsub_tools:publish_sample_content(?DEFAULT_TOPIC_NAME,
										      ?TOPIC_SERVICE_ADDR,
										      <<"abc123">>, Alice, sample_two),

			  RecvdItemId = pubsub_tools:get_publish_response_item_id(RecvdStanza1),
			   io:format(" Received ItemId: ~n~p~n",[RecvdItemId]),
			   %% see example 100

			   %% ------retraction test------

			   RetractFromNode = pubsub_helper:retract_from_node_stanza(?DEFAULT_TOPIC_NAME, RecvdItemId),
			   IqId2 = <<"retract1">>,
			   RetractFromNodeIq  =  pubsub_helper:iq_with_id(set, IqId2, ?TOPIC_SERVICE_ADDR, Alice,  [RetractFromNode]),
			   ReportString =  " Request RetractFromNodeIq: ~n~p~n",
			   ct:pal(ReportString, [exml:to_binary(RetractFromNodeIq)]),
			   io:format(ReportString, [RetractFromNodeIq]),
			   escalus:send(Alice, RetractFromNodeIq),
			   {true, _RecvdStanza} = pubsub_tools:wait_for_stanza_and_match_result_iq(Alice, IqId2, ?TOPIC_SERVICE_ADDR)
			   %% see example 115
		  end).

    
%% XEP0060---6.1.1 Subscribe to node request --------------------------------------------
%% Note: it is the OWNER and PUBLISHER Alice who is subscribing...
%% This is probably a corner case - typically owner is auto-subscribed to node he created.
%% Such a test should not faild anyway.
request_to_subscribe_to_node_by_owner_success(Config) ->
     escalus:story(Config, [1],
		   fun(Alice) ->
			   pubsub_tools:subscribe_by_user(Alice, ?DEFAULT_TOPIC_NAME, ?TOPIC_SERVICE_ADDR),
			   %% %% see example 33
			   {true, _RecvdStanza} = pubsub_tools:unsubscribe_by_user(Alice, ?DEFAULT_TOPIC_NAME,  ?TOPIC_SERVICE_ADDR)
		   end).

request_to_subscribe_to_node_success(Config) ->
     escalus:story(Config, [{bob,1}],
		   fun(Bob) ->
			   pubsub_tools:subscribe_by_user(Bob, ?DEFAULT_TOPIC_NAME, ?TOPIC_SERVICE_ADDR),
			   %% %% see example 101    
			   {true, _RecvdStanza} = pubsub_tools:unsubscribe_by_user(Bob, ?DEFAULT_TOPIC_NAME,  ?TOPIC_SERVICE_ADDR)
		   end).



%% XEP0060---6.5.2 Request all items from node--------------------------------------------------
%% Bob is requesting all items in open access mode (no subscription)
request_all_items_from_node_success(Config) ->
     escalus:story(Config, [{bob,1}],
		   fun(Bob) ->
			   RequestAllItems = pubsub_helper:create_request_allitems_stanza(?DEFAULT_TOPIC_NAME),
			   DestinationNode = ?TOPIC_SERVICE_ADDR,
			   Id = <<"items1">>,
			   RequestAllItemsIq  =  pubsub_helper:iq_with_id(get, Id, DestinationNode, Bob,  [RequestAllItems]),
			   ct:pal(" Request all items (Bob): ~n~n~p~n",[exml:to_binary(RequestAllItemsIq)]),
			   escalus:send(Bob, RequestAllItemsIq),
			   {true, Res1} = pubsub_tools:wait_for_stanza_and_match_result_iq(Bob, Id, DestinationNode), %%wait for subscr. confirmation
       			   ct:pal(" Requested items for Bob: ~n~n~p~n",[exml:to_binary(Res1)])
			   
			   %% see example 78
		   end).

    

%% XEP0060---6.2.1 Unubscribe from node request --------------------------------------------
%% Alice as owner might want to stop subscribing to its own node. This should not failed but does not
%% make much sence if owner is auto-subscribed or/and where subscribtions are presence based.
request_to_unsubscribe_from_node_by_owner_success(Config) ->
     escalus:story(Config, [1],
		   fun(Alice) ->
			   {true, _RecvdStanza} = pubsub_tools:unsubscribe_by_user(Alice, ?DEFAULT_TOPIC_NAME,  ?TOPIC_SERVICE_ADDR)
			   %% UnubscribeFromNode = pubsub_helper:create_unsubscribe_from_node_stanza(?DEFAULT_TOPIC_NAME, Alice),
			   %% DestinationNode = ?TOPIC_SERVICE_ADDR,
			   %% Id = <<"unsub1">>,
			   %% UnSubscribeFromNodeIq  = pubsub_helper:iq_with_id(set, Id, DestinationNode, Alice,  [UnubscribeFromNode]),
			   %% ct:pal(" Request UnSubscribeFromNodeIq: ~n~n~p~n",[exml:to_binary(UnSubscribeFromNodeIq)]),
			   %% escalus:send(Alice, UnSubscribeFromNodeIq),
			   %% {true, _RecvdStanza} = pubsub_tools:wait_for_stanza_and_match_result_iq(Alice, Id, DestinationNode)
		   end).



%% XEP0060---8.4.1 Delete node request --------------------------------------------
%% Alice, as Owner requests the deletion of her node.
request_to_delete_node_success(Config) ->
     escalus:story(Config, [1], 
		   fun(Alice) ->
			   DeleteNode = pubsub_helper:delete_node_stanza(?DEFAULT_TOPIC_NAME),
			   DestinationNode = ?TOPIC_SERVICE_ADDR,
			   Id = <<"delete1">>,
			   DeleteNodeIq  =  pubsub_helper:iq_with_id(set, Id, DestinationNode, Alice,  [DeleteNode]),
			   ct:pal(" Request DeleteNodeIq: ~n~n~p~n",[exml:to_binary(DeleteNodeIq)]),
			   escalus:send(Alice, DeleteNodeIq),
			   {true, _RecvdStanza} = pubsub_tools:wait_for_stanza_and_match_result_iq(Alice, Id, DestinationNode)
			   %% example 156
		   end).


%% XEP0060---8.8.1 retrieve subscriptions list  --------------------------------------------
%% Alice, as Owner wants to know what entities subscribed to her node
request_to_retrieve_subscription_list_by_owner_success(Config) ->
     escalus:story(Config, [1], 
		   fun(Alice) ->
			   RetrieveSubscriptions = pubsub_helper:retrieve_subscriptions_stanza(?DEFAULT_TOPIC_NAME),
			   DestinationNode = ?TOPIC_SERVICE_ADDR,
			   Id = <<"subman1">>,
			   RetrieveSubscriptionsId = pubsub_helper:iq_with_id(get, Id, DestinationNode, Alice, [RetrieveSubscriptions]),
			   ct:pal(" Request RetrieveSubscriptionsId: ~n~n~p~n",[exml:to_binary(RetrieveSubscriptionsId )]),
			   escalus:send(Alice, RetrieveSubscriptionsId ),
			   {true, RecvdStanza} = pubsub_tools:wait_for_stanza_and_match_result_iq(Alice, Id, DestinationNode),
			   ReportString = "Subscriptions received :",
			   io:format(ReportString ++ "~n~n~p",[RecvdStanza]),
  	           	   ct:pal(ReportString ++ "~n~n~s",[exml:to_binary(RecvdStanza)])
			   %% example 183
		   end).






multiple_notifications_success(Config) ->
 escalus:story(Config, [{alice,1},{bob,1},{geralt,1},{carol,1}],
		   fun(Alice, Bob, Geralt, Carol) ->
			   pubsub_tools:subscribe_by_user(Bob, ?DEFAULT_TOPIC_NAME, ?TOPIC_SERVICE_ADDR),
			   {true, _RecvdStanza} = pubsub_tools:publish_sample_content(?DEFAULT_TOPIC_NAME,
										     ?TOPIC_SERVICE_ADDR,
										     <<"xyz123">>, Alice, sample_three),

			   StanzaGot = escalus:wait_for_stanza(Bob),
			   io:format(" --- bob got stanzas --- ~n~p~n", [StanzaGot])

		   end).




