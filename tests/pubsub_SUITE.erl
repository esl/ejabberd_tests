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

%% pubsub_full_cycle
%% case where owner creates a node, publishes to it and gets what he published. It's a round-trip one-user case. 




all() ->
    [{group, pubsub_full_cycle_two_users}].

groups() ->
    [{pubsub_full_cycle_two_users, [sequence], [
						request_to_create_node_success,
						request_to_publish_to_node_success, 
						request_to_subscribe_to_node_success,
						request_to_subscribe_to_node_by_owner_success,
						request_all_items_from_node_success,
						%%request_to_retract_item_success,
						request_to_retrieve_subscription_list_by_owner_success,
						request_to_unsubscribe_from_node_by_owner_success,
						request_to_delete_node_success
				     ]}].


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
    %% Config2 = escalus:create_users(Config,{by_name, [bob]}),
    escalus:init_per_testcase(request_to_subscribe_to_node_success, Config);

init_per_testcase(_TestName, Config) ->
    escalus:init_per_testcase(_TestName, Config).

end_per_testcase(request_to_subscribe_to_node_success, Config) ->
    %% Config2 = escalus:delete_users(Config,{by_name, [bob]}),
    escalus:end_per_testcase(request_to_subscribe_to_node_success, Config);

end_per_testcase(_TestName, Config) ->
    escalus:end_per_testcase(_TestName, Config).


%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

-define (DEST_NODE_ADDR, <<"pubsub.localhost">>).
-define (DEFAULT_TOPIC_NAME, <<"princely_musings">>).
   
%% XEP0060---8.1.1 Create a node with default configuration ---------------------------
request_to_create_node_success(Config) ->
    escalus:story(Config, [1],
		   fun(Alice) ->
			   PubSubCreate = pubsub_helper:create_specific_node_stanza(?DEFAULT_TOPIC_NAME),
			   PubSub = pubsub_helper:pubsub_stanza([PubSubCreate], ?NS_PUBSUB),
			   DestinationNode = ?DEST_NODE_ADDR,
			   Id = <<"create1">>,
			   PubSubCreateIq  =  pubsub_helper:iq_with_id(set, Id, DestinationNode, Alice,  [PubSub]),
			   ct:pal(" Request PubSubCreateIq: ~n~n~p~n",[exml:to_binary(PubSubCreateIq)]),
			   escalus:send(Alice, PubSubCreateIq),
			   {true, _RecvdStanza} = pubsub_tools:wait_for_stanza_and_match_iq(Alice, Id, DestinationNode)
			   %% example 131
		   end).

%% XEP0060---7.1.1 Request to publish to a node -----------------------------------------
%% item with ID=abc123 is being published - use this id for retract and other tests in this group
request_to_publish_to_node_success(Config) ->
     escalus:story(Config, [1],
		   fun(Alice) ->
			   PublishToNode = pubsub_helper:create_publish_node_content_stanza(?DEFAULT_TOPIC_NAME, <<"abc123">>),
			   DestinationNode = ?DEST_NODE_ADDR,
			   Id = <<"publish1">>,
			   PublishToNodeIq  =  pubsub_helper:iq_with_id(set, Id, DestinationNode, Alice,  [PublishToNode]),
			   ct:pal(" Request PublishToNodeIq: ~n~n~p~n",[exml:to_binary(PublishToNodeIq)]),
			   escalus:send(Alice, PublishToNodeIq),
			   {true, _RecvdStanza} = pubsub_tools:wait_for_stanza_and_match_iq(Alice, Id, DestinationNode)
			   %% see example 100
		   end).

%% XEP0060---7.2.1 Request delete item from node -----------------------------------------
%% Alice as Owner and Publisher might want to delete previously published item
%% In this case it is item with ID=abc123
request_to_retract_item_success(Config) ->
     escalus:story(Config, [1],
		   fun(Alice) ->
			   RetractFromNode = pubsub_helper:retract_from_node_stanza(?DEFAULT_TOPIC_NAME, <<"abc123">>),
			   DestinationNode = ?DEST_NODE_ADDR,
			   Id = <<"retract1">>,
			   RetractFromNodeIq  =  pubsub_helper:iq_with_id(set, Id, DestinationNode, Alice,  [RetractFromNode]),
			   ct:pal(" Request RetractFromNodeIq: ~n~n~p~n",[exml:to_binary(RetractFromNodeIq)]),
			   escalus:send(Alice, RetractFromNodeIq),
			   {true, _RecvdStanza} = pubsub_tools:wait_for_stanza_and_match_iq(Alice, Id, DestinationNode)
			   %% see example 115
		   end).


    
%% XEP0060---6.1.1 Subscribe to node request --------------------------------------------
%% Note: it is the OWNER and PUBLISHER Alice who is subscribing...
%% This is probably a corner case - typically owner is auto-subscribed to node he created.
%% Such a test should not faild anyway.
request_to_subscribe_to_node_by_owner_success(Config) ->
     escalus:story(Config, [1],
		   fun(Alice) ->
			   pubsub_tools:subscribe_by_user(Alice, ?DEFAULT_TOPIC_NAME, ?DEST_NODE_ADDR)
			   %% %% see example 33
		   end).

request_to_subscribe_to_node_success(Config) ->
     escalus:story(Config, [{bob,1}],
		   fun(Bob) ->
			   pubsub_tools:subscribe_by_user(Bob, ?DEFAULT_TOPIC_NAME, ?DEST_NODE_ADDR)
			   %% %% see example 101    
		   end).



%% XEP0060---6.5.2 Request all items from node--------------------------------------------------
%% Bob is requesting all items in open access mode (no subscription)
request_all_items_from_node_success(Config) ->
     escalus:story(Config, [{bob,1}],
		   fun(Bob) ->
			   RequestAllItems = pubsub_helper:create_request_allitems_stanza(?DEFAULT_TOPIC_NAME),
			   DestinationNode = ?DEST_NODE_ADDR,
			   Id = <<"items1">>,
			   RequestAllItemsIq  =  pubsub_helper:iq_with_id(get, Id, DestinationNode, Bob,  [RequestAllItems]),
			   ct:pal(" Request all items (Bob): ~n~n~p~n",[exml:to_binary(RequestAllItemsIq)]),
			   escalus:send(Bob, RequestAllItemsIq),
			   {true, Res1} = pubsub_tools:wait_for_stanza_and_match_iq(Bob, Id, DestinationNode), %%wait for subscr. confirmation
       			   ct:pal(" Requested items for Bob: ~n~n~p~n",[exml:to_binary(Res1)])
			   
			   %% see example 78
		   end).

    

%% XEP0060---6.2.1 Unubscribe from node request --------------------------------------------
%% Alice as owner might want to stop subscribing to its own node. This should not failed but does not
%% make much sence if owner is auto-subscribed or/and where subscribtions are presence based.
request_to_unsubscribe_from_node_by_owner_success(Config) ->
     escalus:story(Config, [1],
		   fun(Alice) ->
			   UnubscribeFromNode = pubsub_helper:create_unsubscribe_from_node_stanza(?DEFAULT_TOPIC_NAME, Alice),
			   DestinationNode = ?DEST_NODE_ADDR,
			   Id = <<"unsub1">>,
			   UnSubscribeFromNodeIq  = pubsub_helper:iq_with_id(set, Id, DestinationNode, Alice,  [UnubscribeFromNode]),
			   ct:pal(" Request UnSubscribeFromNodeIq: ~n~n~p~n",[exml:to_binary(UnSubscribeFromNodeIq)]),
			   escalus:send(Alice, UnSubscribeFromNodeIq),
			   {true, _RecvdStanza} = pubsub_tools:wait_for_stanza_and_match_iq(Alice, Id, DestinationNode)
		   end).



%% XEP0060---8.4.1 Delete node request --------------------------------------------
%% Alice, as Owner requests the deletion of her node.
request_to_delete_node_success(Config) ->
     escalus:story(Config, [1], 
		   fun(Alice) ->
			   DeleteNode = pubsub_helper:delete_node_stanza(?DEFAULT_TOPIC_NAME),
			   DestinationNode = ?DEST_NODE_ADDR,
			   Id = <<"delete1">>,
			   DeleteNodeIq  =  pubsub_helper:iq_with_id(set, Id, DestinationNode, Alice,  [DeleteNode]),
			   ct:pal(" Request DeleteNodeIq: ~n~n~p~n",[exml:to_binary(DeleteNodeIq)]),
			   escalus:send(Alice, DeleteNodeIq),
			   {true, _RecvdStanza} = pubsub_tools:wait_for_stanza_and_match_iq(Alice, Id, DestinationNode)
			   %% example 156
		   end).


%% XEP0060---8.8.1 retrieve subscriptions list  --------------------------------------------
%% Alice, as Owner wants to know what entities subscribed to her node
request_to_retrieve_subscription_list_by_owner_success(Config) ->
     escalus:story(Config, [1], 
		   fun(Alice) ->
			   RetrieveSubscriptions = pubsub_helper:retrieve_subscriptions_stanza(?DEFAULT_TOPIC_NAME),
			   DestinationNode = ?DEST_NODE_ADDR,
			   Id = <<"subman1">>,
			   RetrieveSubscriptionsId  = pubsub_helper:iq_with_id(get, Id, DestinationNode, Alice,  [RetrieveSubscriptions]),
			   ct:pal(" Request RetrieveSubscriptionsId: ~n~n~p~n",[exml:to_binary(RetrieveSubscriptionsId )]),
			   escalus:send(Alice, RetrieveSubscriptionsId ),
			   {true, RecvdStanza} = pubsub_tools:wait_for_stanza_and_match_iq(Alice, Id, DestinationNode),
			    ct:pal("Subscriptions received: ~s~n",[exml:to_binary(RecvdStanza)])
			   %% example 183
		   end).














