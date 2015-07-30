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
%% multiple user scenarios executed in sequence, to check the core pubsub functionality and
%% if there is no hidden state on server side between the tests.

all() -> [
	  {group, pubsub_full_cycle},
	  {group, notification_subscription_tests}
	 ].

groups() ->  [
	      {pubsub_full_cycle, [sequence], [
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
	      {notification_subscription_tests, [sequence], [
							     multiple_notifications_success, 
							     temporary_subscription_test,
							     subscription_change_notification_test
							    ]}
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

init_per_group(notification_subscription_tests, Config) ->
    escalus:create_users(Config,{by_name, [alice, bob, geralt, carol]});

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config,{by_name, [alice, bob, geralt, carol]}).

end_per_group(notification_subscription_tests, Config) ->
    escalus:delete_users(Config,{by_name, [alice, bob, geralt, carol]});

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config,{by_name, [alice, bob, geralt, carol]}).

init_per_testcase(request_to_subscribe_to_node_success, Config) ->
    escalus:init_per_testcase(request_to_subscribe_to_node_success, Config);

init_per_testcase(CaseName = request_to_unsubscribe_from_node_by_owner_succes, Config) ->
    %%Config1 = escalus:create_users(Config,{by_name, [alice, bob, geralt, carol]}),
    escalus:init_per_testcase(CaseName, Config);

init_per_testcase(_TestName, Config) ->
    escalus:init_per_testcase(_TestName, Config).

end_per_testcase(request_to_subscribe_to_node_success, Config) ->
    escalus:end_per_testcase(request_to_subscribe_to_node_success, Config);

end_per_testcase(CaseName = request_to_unsubscribe_from_node_by_owner_succes, Config) ->
    %%Config1 = escalus:delete_users(Config,{by_name, [alice, bob, geralt, carol]}),
    escalus:end_per_testcase(CaseName, Config);

end_per_testcase(_TestName, Config) ->
    escalus:end_per_testcase(_TestName, Config).



-define (NODE_ADDR, <<"pubsub.localhost">>).
-define (NODE_NAME, <<"princely_musings">>).
   
%% XEP0060---8.1.1 Create a node with default configuration ---------------------------
request_to_create_node_success(Config) ->
    escalus:story(Config, [1],
		   fun(Alice) ->
			   {true, _RecvdStanza} = pubsub_tools:create_node(Alice, ?NODE_ADDR, ?NODE_NAME)
			   %% example 131
		   end).

%% XEP0060---7.1.1 Request to publish to a node -----------------------------------------
%% item with ID=abc123 is being published - use this id for retract and other tests in this group
request_to_publish_to_node_success(Config) ->
     escalus:story(Config, [1],
		   fun(Alice) ->
			   {true, _RecvdStanza} = pubsub_tools:publish_sample_content(?NODE_NAME,
										     ?NODE_ADDR,
										     <<"abc123">>, Alice, sample_one)
			   %% see example 100
		   end).


users_get_notified_success(Config) ->
 escalus:story(Config, [{alice,1},{bob,1}],
		   fun(Alice, Bob) ->
			   pubsub_tools:subscribe_by_user(Bob, ?NODE_NAME, ?NODE_ADDR),
			   {true, _RecvdStanza} = pubsub_tools:publish_sample_content(?NODE_NAME,
										     ?NODE_ADDR,
										     <<"xyz123">>,
										      Alice,
										      sample_three),

			   StanzaGot1 = escalus:wait_for_stanza(Bob),
			   io:format(" --- bob got stanza1 --- ~n~p~n", [StanzaGot1]),

			   {true, _RecvdStanza2} = pubsub_tools:publish_sample_content(?NODE_NAME,
										     ?NODE_ADDR,
										     <<"abc123">>,
										       Alice,
										       sample_one),
			   StanzaGot2 = escalus:wait_for_stanza(Bob),
			   io:format(" --- bob got stanza2 --- ~n~p~n", [StanzaGot2])

		   end).

%% XEP0060---7.2.1 Request delete item from node -----------------------------------------
%% Alice as Owner and Publisher might want to delete previously published item
%% In this case it is item with ID=abc123
request_to_retract_item_success(Config) ->
    escalus:story(Config, [1],
		  fun(Alice) ->
			   {true, RecvdStanza1} = pubsub_tools:publish_sample_content(?NODE_NAME,
										      ?NODE_ADDR,
										      <<"abc123">>,
										      Alice,
										      sample_two),

			  RecvdItemId = pubsub_tools:get_publish_response_item_id(RecvdStanza1),
			   io:format(" Received ItemId: ~n~p~n",[RecvdItemId]),
			   %% see example 100

			   %% ------retraction test------

			  RetractFromNode = pubsub_helper:retract_from_node_stanza(?NODE_NAME,
										   RecvdItemId),
			   IqId2 = <<"retract1">>,
			  RetractFromNodeIq  =  pubsub_helper:iq_with_id(set,
									 IqId2,
									 ?NODE_ADDR,
									 Alice,
									 [RetractFromNode]),
			   ReportString =  " Request RetractFromNodeIq: ~n~p~n",
			   ct:pal(ReportString, [exml:to_binary(RetractFromNodeIq)]),
			   io:format(ReportString, [RetractFromNodeIq]),
			   escalus:send(Alice, RetractFromNodeIq),
			   {true, _RecvdStanza} = pubsub_tools:wait_for_stanza_and_match_result_iq(Alice, IqId2, ?NODE_ADDR)
			   %% see example 115
		  end).

    
%% XEP0060---6.1.1 Subscribe to node request --------------------------------------------
%% Note: it is the OWNER and PUBLISHER Alice who is subscribing...
%% This is probably a corner case - typically owner is auto-subscribed to node he created.
%% Such a test should not faild anyway.
request_to_subscribe_to_node_by_owner_success(Config) ->
     escalus:story(Config, [1],
		   fun(Alice) ->
			   pubsub_tools:subscribe_by_user(Alice, ?NODE_NAME, ?NODE_ADDR),
			   %% %% see example 33
			   {true, _RecvdStanza} = pubsub_tools:unsubscribe_by_user(Alice, ?NODE_NAME,  ?NODE_ADDR)
		   end).

request_to_subscribe_to_node_success(Config) ->
     escalus:story(Config, [{bob,1}],
		   fun(Bob) ->
			   pubsub_tools:subscribe_by_user(Bob, ?NODE_NAME, ?NODE_ADDR),
			   %% %% see example 101    
			   {true, _RecvdStanza} = pubsub_tools:unsubscribe_by_user(Bob, ?NODE_NAME,  ?NODE_ADDR)
		   end).



%% XEP0060---6.5.2 Request all items from node--------------------------------------------------
%% Bob is requesting all items in open access mode (no subscription)
request_all_items_from_node_success(Config) ->
     escalus:story(Config, [{bob,1}],
		   fun(Bob) ->
			   RequestAllItems = pubsub_helper:create_request_allitems_stanza(?NODE_NAME),
			   DestinationNode = ?NODE_ADDR,
			   Id = <<"items1">>,
			   RequestAllItemsIq  =  pubsub_helper:iq_with_id(get, Id, DestinationNode, Bob,  [RequestAllItems]),
			   ct:pal(" Request all items (Bob): ~n~n~p~n",[exml:to_binary(RequestAllItemsIq)]),
			   escalus:send(Bob, RequestAllItemsIq),
			   {true, Res1} = pubsub_tools:wait_for_stanza_and_match_result_iq(Bob, Id, DestinationNode),
       			   ct:pal(" Requested items for Bob: ~n~n~p~n",[exml:to_binary(Res1)])
			   %% see example 78
		   end).

    

%% XEP0060---6.2.1 Unubscribe from node request --------------------------------------------
%% Alice as owner might want to stop subscribing to its own node. This should not failed but does not
%% make much sence if owner is auto-subscribed or/and where subscribtions are presence based.
request_to_unsubscribe_from_node_by_owner_success(Config) ->
     escalus:story(Config, [{alice,1},{bob,1},{geralt,1},{carol,1}],
		   fun(_Alice, Bob, Geralt, Carol) ->
			   pubsub_tools:subscribe_by_user(Bob, ?NODE_NAME, ?NODE_ADDR),
			   pubsub_tools:subscribe_by_user(Geralt, ?NODE_NAME, ?NODE_ADDR),
			   pubsub_tools:subscribe_by_user(Carol, ?NODE_NAME, ?NODE_ADDR),
			   {true, _RecvdStanza} = pubsub_tools:unsubscribe_by_user(Bob, ?NODE_NAME,  ?NODE_ADDR),
			   {true, _RecvdStanza2} = pubsub_tools:unsubscribe_by_user(Geralt, ?NODE_NAME,  ?NODE_ADDR),
			   {true, _RecvdStanza3} = pubsub_tools:unsubscribe_by_user(Carol,  ?NODE_NAME,  ?NODE_ADDR)
		   end).


%% XEP0060---8.4.1 Delete node request --------------------------------------------
%% Alice, as Owner requests the deletion of her node.
request_to_delete_node_success(Config) ->
     escalus:story(Config, [1], 
		   fun(Alice) ->
			   DeleteNode = pubsub_helper:delete_node_stanza(?NODE_NAME),
			   DestinationNode = ?NODE_ADDR,
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
     escalus:story(Config, [{alice,1},{bob,1},{geralt,1}], 
		   fun(Alice,Bob,Geralt) ->
			   %% pubsub_tools:subscribe_by_user(Alice, ?NODE_NAME, ?NODE_ADDR),
			   pubsub_tools:subscribe_by_user(Bob, ?NODE_NAME, ?NODE_ADDR),
			   pubsub_tools:subscribe_by_user(Geralt, ?NODE_NAME, ?NODE_ADDR),

			   {true, RecvdStanza} = pubsub_tools:get_subscription_list_by_owner(Alice, ?NODE_NAME,  ?NODE_ADDR),

			   %% get extracted subscription dictionay content for easy access
			   SubscrListResult = pubsub_tools:get_users_and_subscription_states(RecvdStanza),

			   CurrentEscaulsUserList = element(2, lists:nth(5, Config)),
			   true = check_all_users_in_subscription(SubscrListResult, CurrentEscaulsUserList)
			       
			   %% example 183
		   end).

%% pass dictionary of {Jid, SubscriptionValue} generated from response as in example 183 using function 
%% get_users_and_subscription_states (pubsub_tools).
%% Users list to check is taken from current Config - get the current users list from there. 
check_all_users_in_subscription(SubscriptionListContent, EscalusCurrentUserListToCheck) ->
    JidListToCheck  = lists:map(
			fun(Elem) -> escalus_utils:get_jid(element(1,Elem)) end,
			EscalusCurrentUserListToCheck),

    %% for all tested users find them on response dictionary and find which are
    %% subscribed and which not
    Res = lists:map(
	    fun(Jid) -> 
		   case  dict:is_key(Jid, SubscriptionListContent) of
		       true ->
			   case dict:fetch(Jid, SubscriptionListContent) of
			       <<"subscribed">> -> {Jid, <<"subscribed">>};
			       _ -> {Jid, notsubscribed}
			   end;
		       false  -> {Jid, nosuchuser}
		   end
	    end, JidListToCheck),

    NotSubscribed = lists:filter(
		      fun(Elem) ->
			      {_Jid, Val} = Elem,
			      Val == notsubscribed
		      end, Res),

    [] =:= NotSubscribed.


%% Alice as topic owner publishes two messages and three users get two notifications with payloads and the
%% received messages ID are checked if they match the published ones.
%% next, the subscription list is checked by topic owner and the users unsubsribe explicitely from the
%% topic. Again, subscription list is checked.
multiple_notifications_success(Config) ->
 escalus:story(Config, [{alice,1},{bob,1},{geralt,1},{carol,1}],
		   fun(Alice, Bob, Geralt, Carol) ->
			   TopicName = <<"TABLETS">>,
			   %% first, let Alice create a new topic
			   {true, _RecvdStanza} = pubsub_tools:create_node(Alice, ?NODE_ADDR, TopicName),
			   %% subscribe bunch of users...
			   pubsub_tools:subscribe_by_users([Bob, Geralt, Carol], TopicName, ?NODE_ADDR),

			   %% and publish a dummy message ONE for everyone -----------------
			   {true, PublishedItem1Stanza} = pubsub_tools:publish_sample_content(TopicName,
										     ?NODE_ADDR,
										     <<"dummyFIRST">>, Alice, sample_two),


			   Published_1_ItemId = pubsub_tools:get_publish_response_item_id(PublishedItem1Stanza),
			   io:format(" Published Item 1 Id was : ~n~p~n",[Published_1_ItemId]),


			   %% and publish a dummy message TWO for everyone ----------------			
			   {true, PublishedItem2Stanza} = pubsub_tools:publish_sample_content(TopicName,
										     ?NODE_ADDR,
										     <<"dummySECOND">>, Alice, sample_three),


			   Published_2_ItemId = pubsub_tools:get_publish_response_item_id(PublishedItem2Stanza),
			   io:format(" Published Item 2 Id was : ~n~p~n",[Published_2_ItemId]),

			   true =  Published_1_ItemId =:= wait_and_get_notification_item_id_for_user(Bob),
			   true =  Published_2_ItemId =:= wait_and_get_notification_item_id_for_user(Bob),
			   true =  Published_1_ItemId =:= wait_and_get_notification_item_id_for_user(Geralt),
			   true =  Published_2_ItemId =:= wait_and_get_notification_item_id_for_user(Geralt),
			   true =  Published_1_ItemId =:= wait_and_get_notification_item_id_for_user(Carol),
			   true =  Published_2_ItemId =:= wait_and_get_notification_item_id_for_user(Carol),

			   io:format(" -first try- there should be 3 users subscribed to topic"),

			   {true, RecvdSubscrList1} = pubsub_tools:get_subscription_list_by_owner(Alice, TopicName, ?NODE_ADDR),
			   false = dict:is_empty(pubsub_tools:get_users_and_subscription_states(RecvdSubscrList1)),

			   pubsub_tools:unsubscribe_by_users([Bob, Geralt, Carol], TopicName, ?NODE_ADDR),

			   io:format(" -second try- ...now no one is subscribed"),

			   {true, RecvdSubscrList2} = pubsub_tools:get_subscription_list_by_owner(Alice, TopicName, ?NODE_ADDR),
			   %% get extracted subscription dictionary content for easy access
			   
			   %% nobody should be subscribed - dictionary is empty.
			   true = dict:is_empty(pubsub_tools:get_users_and_subscription_states(RecvdSubscrList2))

		   end).

%% according to 12.4 of XEP-0060 and with assumption this is the DEFAULT setting (no check for configuration entry
%% for temp. subscription)
%% 2 users subscribes, 1 goes offline, his subscription should disappear
temporary_subscription_test(Config) ->
     escalus:story(Config, [{alice,1},{bob,1},{geralt,1}], 
		   fun(Alice,Bob,Geralt) ->
			   TopicName = <<"STRANGENODE">>,
  			   {true, _RecvdStanza} = pubsub_tools:create_node(Alice, ?NODE_ADDR, TopicName),
			   pubsub_tools:subscribe_by_users([Bob, Geralt], TopicName, ?NODE_ADDR),

			   {true, RecvdStanzaBeforeDrop} = pubsub_tools:get_subscription_list_by_owner(Alice, TopicName,  ?NODE_ADDR),
			   SubscrListResultBeforeDrop = pubsub_tools:get_users_and_subscription_states(RecvdStanzaBeforeDrop),
			   CurrentEscaulsUserList = element(2, lists:nth(5, Config)),
			   true = check_all_users_in_subscription(SubscrListResultBeforeDrop, CurrentEscaulsUserList),

			   escalus:send(Bob, escalus_stanza:presence(<<"unavailable">>)),

			   {true, RecvdStanzaAfterDrop} = pubsub_tools:get_subscription_list_by_owner(Alice, TopicName, ?NODE_ADDR),
			   SubscrListResultAfterDrop = pubsub_tools:get_users_and_subscription_states(RecvdStanzaAfterDrop),

			   BobsJid = escalus_utils:get_jid(Bob),
			   io:format(" BobID to check: ~p",[BobsJid]),
			   %% Bob should not be on subscribe list for this node any more.
			   error = dict:find(BobsJid, SubscrListResultAfterDrop)
		   end).

%% 8.8.4 - Notifying Subscribers
%% according to example 194 if there is change in user's subscription status/mode - he should
%% be notified by the server about this fact
%% This case is examined with help of 8.8.3 (no example)
subscription_change_notification_test(Config) ->
     escalus:story(Config, [{alice,1},{geralt,1}], 
		   fun(Alice,Geralt) ->
			   TopicName = <<"IWILLKICKYOUALL">>,
  			   {true, _RecvdStanza} = pubsub_tools:create_node(Alice, ?NODE_ADDR, TopicName),
			   pubsub_tools:subscribe_by_user(Geralt, TopicName, ?NODE_ADDR),

			   {true, _RecvdStanzaBeforeKick} = pubsub_tools:get_subscription_list_by_owner(Alice, TopicName,  ?NODE_ADDR),

			   ChangeData = [{escalus_utils:get_short_jid(Geralt), <<"none">>}],


			   {_Result, _ResultStanza} = pubsub_tools:request_subscription_changes_by_owner(Alice, TopicName, ?NODE_ADDR, ChangeData)

			   %%UserStanzaGot = escalus:wait_for_stanza(Geralt),			   
			   %%io:format(" ------  Geralt  got stanza ------ ~n~p~n", [UserStanzaGot]),


	
			   %% SubscrListResultBeforeDrop = pubsub_tools:get_users_and_subscription_states(RecvdStanzaBeforeKick),
			   %% CurrentEscaulsUserList = element(2, lists:nth(5, Config)),
			   %% true = check_all_users_in_subscription(SubscrListResultBeforeDrop, CurrentEscaulsUserList),

			   %% escalus:send(Bob, escalus_stanza:presence(<<"unavailable">>)),

			   %% {true, RecvdStanzaAfterDrop} = pubsub_tools:get_subscription_list_by_owner(Alice, TopicName, ?NODE_ADDR),
			   %% SubscrListResultAfterDrop = pubsub_tools:get_users_and_subscription_states(RecvdStanzaAfterDrop),

			   %% BobsJid = escalus_utils:get_jid(Bob),
			   %% io:format(" BobID to check: ~p",[BobsJid]),
			   %% %% Bob should not be on subscribe list for this node any more.
			   %% error = dict:find(BobsJid, SubscrListResultAfterDrop)
		   end).

%%dupa

%% call when notification with message payload is expected. Call many times for many messages to consume
%% all of them (typical case).
wait_and_get_notification_item_id_for_user(User) ->
    %% UserJid = escalus_utils:get_jid(User),
    UserStanzaGot = escalus:wait_for_stanza(User),
    %% io:format(" ------  ~p got stanza ------ ~n~p~n", [UserJid, UserStanzaGot]),
    ItemListUser = pubsub_tools:get_event_notification_items_list(UserStanzaGot),
    %% io:format(" and ~p s items in stanza are: ~n~p~n", [UserJid,ItemListUser]),
    ItemsIdsUser = pubsub_tools:get_items_ids(ItemListUser),
    io:format("-- received ids: ~n~p~n", [ItemsIdsUser]),
    hd(ItemsIdsUser).





