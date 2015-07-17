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
						%%request_to_retrieve_subscription_list_by_owner_success,
						request_to_unsubscribe_from_node_by_owner_success
						%%request_to_delete_node_success
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



%% ---------------- PUB SUB STANZAS -------------



pubsub_stanza(Children, NS) ->
    #xmlel{name = <<"pubsub">>,
	     attrs = [{<<"xmlns">>, NS} ],
	     children = Children  }.

create_specific_node_stanza(NodeName) ->
    #xmlel{
       name = <<"create">>,
       attrs = [{<<"node">>, NodeName}] }.

iq_with_id(TypeAtom, Id, To, From, Body) ->
    S1 = escalus_stanza:iq(To, atom_to_binary(TypeAtom, latin1), Body),
    iq_set_get_rest(S1, Id, From).

iq_set_get_rest(SrcIq, Id, From) ->
    S2 = escalus_stanza:set_id(SrcIq, Id),							
    escalus_stanza:from(S2, escalus_utils:get_jid(From)).


entry_body_sample1() ->
    [
     #xmlel{name = <<"title">>, children  = [ #xmlcdata{content=[<<"The title of content.">>]}]},
     #xmlel{name = <<"summary">>, children= [ #xmlcdata{content=[<<"To be or not to be...">>]}]}
    ].

entry_body_sample_debice_id() ->
    [
     #xmlel{name = <<"DEVICE_ID">>, children  = [ #xmlcdata{content=[<<"2F:AB:28:FF">>]}]}
    ].


%% provide EntryBody as list of anything compliant with exml entity records.
publish_entry(EntryBody) ->
    #xmlel{
       name = <<"entry">>,
       attrs = [{<<"xmlns">>, <<"http://www.w3.org/2005/Atom">>}],
       children = case EntryBody of 
		      #xmlel{} -> EntryBody;
		      _ -> entry_body_sample1()
		  end
      }.

publish_item(ItemId, PublishEntry) ->
    #xmlel{
       name = <<"item">>,
       attrs = [{<<"id">>, ItemId}],
       children = case PublishEntry of
		      #xmlel{} ->
			  PublishEntry;
		      _ ->
			publish_entry([])
		   end
      }.

sample_publish_node_with_content(NodeName, ItemToPublish) ->
    #xmlel{
       name = <<"publish">>,
       attrs = [{<<"node">>, NodeName}],
       children = case ItemToPublish of
		      #xmlel{} -> 
			  ItemToPublish;
		      _ -> 
			  publish_item(<<"abc123">>, [])
		  end
      }.

create_publish_node_content_stanza(NodeName, ItemId) ->
    PublishEntry = publish_entry([]),
    ItemTopublish = publish_item(ItemId, PublishEntry),
    PublNode = sample_publish_node_with_content(NodeName, ItemTopublish),
    pubsub_stanza([PublNode], ?NS_PUBSUB).


retract_from_node_stanza(NodeName, ItemId) ->
    ItemToRetract = #xmlel{name = <<"item">>, attrs=[{<<"id">>, ItemId}], children=[]},
    RetractNode =  #xmlel{name = <<"retract">>, attrs=[{<<"node">>, NodeName}], children=[ItemToRetract]},
    pubsub_stanza([RetractNode], ?NS_PUBSUB).

    

%% ------------ subscribe - unscubscribe -----------


create_subscribe_node_stanza(NodeName, From) ->
    SubsrNode = create_sub_unsubscribe_from_node_stanza(NodeName, From, <<"subscribe">>),
    pubsub_stanza([SubsrNode], ?NS_PUBSUB).

create_request_allitems_stanza(NodeName) ->
    AllItems = #xmlel{name = <<"items">>, attrs=[{<<"node">>, NodeName}]},
    pubsub_stanza([AllItems], ?NS_PUBSUB).

create_unsubscribe_from_node_stanza(NodeName, From) ->
    UnsubsrNode = create_sub_unsubscribe_from_node_stanza(NodeName, From, <<"unsubscribe">>),
    pubsub_stanza([UnsubsrNode], ?NS_PUBSUB).

create_sub_unsubscribe_from_node_stanza(NodeName, From, SubUnsubType) ->
    #xmlel{name = SubUnsubType,
	   attrs = [
		    {<<"node">>, NodeName},
		    {<<"jid">>, escalus_utils:get_jid(From)}]
	  }.

%% ----end----- subscribe - unscubscribe -----------

delete_node_stanza(NodeName) ->
    DelNode = #xmlel{name = <<"delete">>,
		      attrs = [{<<"node">>, NodeName}]
		      },
    pubsub_stanza([DelNode], ?NS_PUBSUB_OWNER).


retrieve_subscriptions_stanza(NodeName) ->
    RetrieveNode = #xmlel{name = <<"subscriptions">>,
		      attrs = [{<<"node">>, NodeName}]
		      },
    pubsub_stanza([RetrieveNode], ?NS_PUBSUB_OWNER).



%% ---END---------- PUB SUB STANZAS -------------

-define (DEST_NODE_ADDR, <<"pubsub.localhost">>).
-define (DEFAULT_TOPIC_NAME, <<"princely_musings">>).
   
%% XEP0060---8.1.1 Create a node with default configuration ---------------------------
request_to_create_node_success(Config) ->
    escalus:story(Config, [1],
		   fun(Alice) ->
			   PubSubCreate = create_specific_node_stanza(?DEFAULT_TOPIC_NAME),
			   PubSub = pubsub_stanza([PubSubCreate], ?NS_PUBSUB),
			   DestinationNode = ?DEST_NODE_ADDR,
			   Id = <<"create1">>,
			   PubSubCreateIq  =  iq_with_id(set, Id, DestinationNode, Alice,  [PubSub]),
			   ct:pal(" Request PubSubCreateIq: ~n~n~p~n",[exml:to_binary(PubSubCreateIq)]),
			   escalus:send(Alice, PubSubCreateIq),
			   {true, _RecvdStanza} = wait_for_stanza_and_match_iq(Alice, Id, DestinationNode)
			   %% example 131
		   end).

%% XEP0060---7.1.1 Request to publish to a node -----------------------------------------
%% item with ID=abc123 is being published - use this id for retract and other tests in this group
request_to_publish_to_node_success(Config) ->
     escalus:story(Config, [1],
		   fun(Alice) ->
			   PublishToNode = create_publish_node_content_stanza(?DEFAULT_TOPIC_NAME, <<"abc123">>),
			   DestinationNode = ?DEST_NODE_ADDR,
			   Id = <<"publish1">>,
			   PublishToNodeIq  =  iq_with_id(set, Id, DestinationNode, Alice,  [PublishToNode]),
			   ct:pal(" Request PublishToNodeIq: ~n~n~p~n",[exml:to_binary(PublishToNodeIq)]),
			   escalus:send(Alice, PublishToNodeIq),
			   {true, _RecvdStanza} = wait_for_stanza_and_match_iq(Alice, Id, DestinationNode)
			   %% see example 100
		   end).

%% XEP0060---7.2.1 Request delete item from node -----------------------------------------
%% Alice as Owner and Publisher might want to delete previously published item
%% In this case it is item with ID=abc123
request_to_retract_item_success(Config) ->
    %% dupa, sypie sie
     escalus:story(Config, [1],
		   fun(Alice) ->
			   RetractFromNode = retract_from_node_stanza(?DEFAULT_TOPIC_NAME, <<"abc123">>),
			   DestinationNode = ?DEST_NODE_ADDR,
			   Id = <<"retract1">>,
			   RetractFromNodeIq  =  iq_with_id(set, Id, DestinationNode, Alice,  [RetractFromNode]),
			   ct:pal(" Request RetractFromNodeIq: ~n~n~p~n",[exml:to_binary(RetractFromNodeIq)]),
			   escalus:send(Alice, RetractFromNodeIq),
			   {true, _RecvdStanza} = wait_for_stanza_and_match_iq(Alice, Id, DestinationNode)
			   %% see example 115
		   end).


    
%% XEP0060---6.1.1 Subscribe to node request --------------------------------------------
%% Note: it is the OWNER and PUBLISHER Alice who is subscribing...
%% This is probably a corner case - typically owner is auto-subscribed to node he created.
%% Such a test should not faild anyway.
request_to_subscribe_to_node_by_owner_success(Config) ->
     escalus:story(Config, [1],
		   fun(Alice) ->
			   subscribe_by_user(Alice, ?DEFAULT_TOPIC_NAME, ?DEST_NODE_ADDR)
			   %% %% see example 33
		   end).

request_to_subscribe_to_node_success(Config) ->
     escalus:story(Config, [{bob,1}],
		   fun(Bob) ->
			   subscribe_by_user(Bob, ?DEFAULT_TOPIC_NAME, ?DEST_NODE_ADDR)
			   %% %% see example 101    
		   end).

subscribe_by_user(User, NodeName, NodeAddress) ->
    SubscribeToNode = create_subscribe_node_stanza(NodeName, User),
    UserName = escalus_utils:get_username(User),
    Id = <<UserName/binary,<<"bobsub1">>/binary>>,
    SubscribeToNodeIq  =  iq_with_id(set, Id, NodeAddress, User,  [SubscribeToNode]),
    ct:pal(" Request SubscribeToNodeIq from user ~p: ~n~p~n",[UserName, exml:to_binary(SubscribeToNodeIq)]),
    escalus:send(User, SubscribeToNodeIq),
    {true, RecvdStanza} = wait_for_stanza_and_match_iq(User, Id, NodeAddress), %%wait for subscr. confirmation
    ct:pal("Subscriptions received by ~p: ~s~n",[UserName, exml:to_binary(RecvdStanza)]),
    is_subscription_for_jid_pred(RecvdStanza, User).


%% XEP0060---6.5.2 Request all items from node--------------------------------------------------
%% Bob is requesting all items in open access mode (no subscription)
request_all_items_from_node_success(Config) ->
     escalus:story(Config, [{bob,1}],
		   fun(Bob) ->
			   RequestAllItems = create_request_allitems_stanza(?DEFAULT_TOPIC_NAME),
			   DestinationNode = ?DEST_NODE_ADDR,
			   Id = <<"items1">>,
			   RequestAllItemsIq  =  iq_with_id(get, Id, DestinationNode, Bob,  [RequestAllItems]),
			   ct:pal(" Request all items (Bob): ~n~n~p~n",[exml:to_binary(RequestAllItemsIq)]),
			   escalus:send(Bob, RequestAllItemsIq),
			   {true, Res1} = wait_for_stanza_and_match_iq(Bob, Id, DestinationNode), %%wait for subscr. confirmation
       			   ct:pal(" Requested items for Bob: ~n~n~p~n",[exml:to_binary(Res1)])
			   
			   %% see example 78
		   end).

    

%% XEP0060---6.2.1 Unubscribe from node request --------------------------------------------
%% Alice as owner might want to stop subscribing to its own node. This should not failed but does not
%% make much sence if owner is auto-subscribed or/and where subscribtions are presence based.
request_to_unsubscribe_from_node_by_owner_success(Config) ->

     escalus:story(Config, [1],
		   fun(Alice) ->
			   UnubscribeFromNode = create_unsubscribe_from_node_stanza(?DEFAULT_TOPIC_NAME, Alice),
			   DestinationNode = ?DEST_NODE_ADDR,
			   Id = <<"unsub1">>,
			   UnSubscribeFromNodeIq  =  iq_with_id(set, Id, DestinationNode, Alice,  [UnubscribeFromNode]),
			   ct:pal(" Request UnSubscribeFromNodeIq: ~n~n~p~n",[exml:to_binary(UnSubscribeFromNodeIq)]),
			   escalus:send(Alice, UnSubscribeFromNodeIq),
			   {true, _RecvdStanza} = wait_for_stanza_and_match_iq(Alice, Id, DestinationNode)
		   end).



%% XEP0060---8.4.1 Delete node request --------------------------------------------
%% Alice, as Owner requests the deletion of her node.
request_to_delete_node_success(Config) ->
    %% dupa , sypie sie

     escalus:story(Config, [1], 
		   fun(Alice) ->
			   DeleteNode = delete_node_stanza(?DEFAULT_TOPIC_NAME),
			   DestinationNode = ?DEST_NODE_ADDR,
			   Id = <<"delete1">>,
			   DeleteNodeIq  =  iq_with_id(set, Id, DestinationNode, Alice,  [DeleteNode]),
			   ct:pal(" Request DeleteNodeIq: ~n~n~p~n",[exml:to_binary(DeleteNodeIq)]),
			   escalus:send(Alice, DeleteNodeIq),
			   {true, _RecvdStanza} = wait_for_stanza_and_match_iq(Alice, Id, DestinationNode)
			   %% example 156
		   end).


%% XEP0060---8.8.1 retrieve subscriptions list  --------------------------------------------
%% Alice, as Owner wants to know what entities subscribed to her node
request_to_retrieve_subscription_list_by_owner_success(Config) ->
    %% dupa, sypie sie

     escalus:story(Config, [1], 
		   fun(Alice) ->
			   RetrieveSubscriptions = retrieve_subscriptions_stanza(?DEFAULT_TOPIC_NAME),
			   DestinationNode = ?DEST_NODE_ADDR,
			   Id = <<"subman1">>,
			   RetrieveSubscriptionsId  =  iq_with_id(get, Id, DestinationNode, Alice,  [RetrieveSubscriptions]),
			   ct:pal(" Request RetrieveSubscriptionsId: ~n~n~p~n",[exml:to_binary(RetrieveSubscriptionsId )]),
			   escalus:send(Alice, RetrieveSubscriptionsId ),
			   {true, RecvdStanza} = wait_for_stanza_and_match_iq(Alice, Id, DestinationNode),
			    ct:pal("Subscriptions received: ~s~n",[exml:to_binary(RecvdStanza)])
			   %% example 183
		   end).



%% ----------------------------- HELPER and DIAGNOSTIC functions -----------------------

%% Checks superficialy is IQ from server  matches the sent id, there is "result" and sender is correct.
wait_for_stanza_and_match_iq(User, Id, DestinationNode) ->
    ResultStanza = escalus:wait_for_stanza(User),
    ct:pal(" Response stanza from server: ~n~n~s~n", [exml:to_binary(ResultStanza)]),

    QueryStanza = escalus_stanza:iq_with_type_id_from(<<"result">>, Id, DestinationNode),
    %%  ct:pal("QueryStanza: ~s~n",[exml:to_binary(QueryStanza)]),

    Result = escalus_pred:is_iq_result(QueryStanza, ResultStanza),
    %%  ct:pal(" result - ~s~n", [exml:to_binary(Result)]),

    {Result, ResultStanza}.

%% generate dummy subscription confirmation from server. Used to test predicate function.
get_subscription_confirmation_stanza() ->
   Subscription = #xmlel{name = <<"subscription">>, attrs=[{<<"jid">>, <<"alice@localhost">>}]},
   PubSub = pubsub_stanza([Subscription], ?NS_PUBSUB),
   DestinationNode = ?DEST_NODE_ADDR,
   Id = <<"sub1">>,
   PubSubItemIq  =  iq_with_id(set, Id, DestinationNode, <<"Alice">>,  [PubSub]),
   %%io:format(" ---- ~n~p~n ", [PubSubItemIq]),
   %%B = exml:to_binary(PubSubItemIq),
   %%io:format(" ---- ~n~p~n ", [B]),
   PubSubItemIq.
	
is_subscription_for_jid_pred(SubscrConfirmation, User) ->
    %% Stanza = get_subscription_confirmation_stanza(),
    %% R = exml_query:path(Stanza, <<"pubsub>>">>),
    R1 = exml_query:subelement(SubscrConfirmation, <<"pubsub">>),
    io:format(" -- ~n~p",[R1]),
    R2 = exml_query:subelement(R1, <<"subscription">>),
    io:format(" ------ ~n~p",[R2]),
    JidOfSubscr = exml_query:attr(R2, <<"jid">>),
    io:format(" -- jid found : ~n~p", [JidOfSubscr]),
    JidOfSubscr =:= escalus_utils:get_jid(User).

















