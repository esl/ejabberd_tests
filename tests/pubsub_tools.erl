%%%===================================================================
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc Suite for testing pubsub features as described in XEP-0060
%%% @Toolsr module - only pubsub specific tools and high level
%%% @wrappers for escalus tool.
%%% @end
%%%===================================================================

-module(pubsub_tools).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").

-export([
	 wait_for_stanza_and_match_result_iq/3,
	 get_subscription_confirmation_stanza/1,
	 get_publish_response_item_id/1,
	 get_event_notification_items_list/1,
	 get_event_notification_subscription_change/1,
	 get_items_ids/1,
	 get_users_and_subscription_states/1,
	 is_subscription_for_jid_pred/3,
	 get_subscription_list_by_owner/3,
	 is_publish_response_matching_item_id/2,
	 publish_sample_content/5,
	 request_subscription_changes_by_owner/4,
	 subscribe_by_user/3,
	 subscribe_by_users/3,
	 unsubscribe_by_user/3,
	 unsubscribe_by_users/3,
	 create_node/3
	]).

%% ----------------------------- HELPER and DIAGNOSTIC functions -----------------------
%% Note ------ functions in this section are not stanza generating functions but:
%% - predicates
%% - high level wrappers for escalus specific to pubsub feature

%% Checks superficialy is IQ from server  matches the sent id, there is "result" and sender is correct.
wait_for_stanza_and_match_result_iq(User, Id, DestinationNode) ->
    ResultStanza = escalus:wait_for_stanza(User),
    ReportString = " RESPONSE stanza: ~n~n",

    io:format(ReportString ++ "~p~n", [ResultStanza]),
    ct:pal(ReportString ++ "~s~n", [exml:to_binary(ResultStanza)]),

    QueryStanza = escalus_stanza:iq_with_type_id_from(<<"result">>, Id, DestinationNode),
    Result = escalus_pred:is_iq_result(QueryStanza, ResultStanza),

    {Result, ResultStanza}.

create_node(User, DestinationNodeAddr, DestinationNodeName) ->
   PubSubCreate = pubsub_helper:create_specific_node_stanza(DestinationNodeName),
   PubSub = pubsub_helper:pubsub_stanza([PubSubCreate], ?NS_PUBSUB),
   Id = <<"create1">>,
   PubSubCreateIq  =  pubsub_helper:iq_with_id(set, Id, DestinationNodeAddr, User,  [PubSub]),
   ct:pal(" REQUEST PubSubCreateIq: ~n~p~n",[exml:to_binary(PubSubCreateIq)]),
   escalus:send(User, PubSubCreateIq),
   {true, _RecvdStanza} = pubsub_tools:wait_for_stanza_and_match_result_iq(User, Id, DestinationNodeAddr).
   %% example 131


%% returns servers' response stanza, according to 8.8.1.1 (owner case)
get_subscription_list_by_owner(User, NodeName, NodeAddr) ->
    RetrieveSubscriptions = pubsub_helper:retrieve_subscriptions_stanza(NodeName),
    Id = <<"subowner">>,
    GetSubscriptionsIq = pubsub_helper:iq_with_id(get, Id, NodeAddr, User, [RetrieveSubscriptions]),
    io:format(" REQUEST RetrieveSubscriptionsByByOwner: ~n~n~p~n",[GetSubscriptionsIq]),
    escalus:send(User, GetSubscriptionsIq ),
    wait_for_stanza_and_match_result_iq(User, Id, NodeAddr).


%% generate dummy subscription confirmation from server. Used to test predicate function.
get_subscription_confirmation_stanza(DestinationNode) ->
   Subscription = #xmlel{name = <<"subscription">>, attrs=[{<<"jid">>, <<"alice@localhost">>}]},
   PubSub = pubsub_helper:pubsub_stanza([Subscription], ?NS_PUBSUB),
   %% DestinationNode = ?DEST_NODE_ADDR,
   Id = <<"sub1">>,
   PubSubItemIq  =  pubsub_helper:iq_with_id(set, Id, DestinationNode, <<"Alice">>,  [PubSub]),
   %%io:format(" ---- ~n~p~n ", [PubSubItemIq]),
   %%B = exml:to_binary(PubSubItemIq),
   %%io:format(" ---- ~n~p~n ", [B]),
   PubSubItemIq.

get_users_and_subscription_states([]) ->
    dict:new();

%% pass subscription list as came from server - users and subscription states will be returned as dictionary where
%% Jid = key , subscriptionState = value}
get_users_and_subscription_states(SubscriptionList) ->
    R = exml_query:attr(SubscriptionList, <<"type">>),
    true = R =:= <<"result">>,
    R1 = exml_query:subelement(SubscriptionList, <<"pubsub">>),
    R2 = exml_query:subelement(R1, <<"subscriptions">>),
    Elems = exml_query:subelements(R2, <<"subscription">>),
    

    Result = lists:map(fun(Element) -> 
			       JidFull = exml_query:attr(Element, <<"jid">>),
			       SubscrState = exml_query:attr(Element, <<"subscription">>),
			       Jid = hd(binary:split(JidFull, <<"/">>)),
			       {Jid, SubscrState}
		       end, Elems),
    
    Res = lists:foldl(
	    fun(Element, NewDict) ->
		    dict:store(element(1, Element), element(2,Element), NewDict)
	    end,
	    dict:new(), 
	    Result),
    
    io:format(" Dump of users subscriptions: ~n~p ", [dict:to_list(Res)]),
    Res.


is_subscription_for_jid_pred(SubscrConfirmation, User, _DestinationNode) ->
    %% Stanza = get_subscription_confirmation_stanza(DestinationNode),
    %% R = exml_query:path(Stanza, <<"pubsub>>">>),
    R1 = exml_query:subelement(SubscrConfirmation, <<"pubsub">>),
    io:format(" -- ~n~p",[R1]),
    R2 = exml_query:subelement(R1, <<"subscription">>),
    io:format(" ------ ~n~p",[R2]),
    JidOfSubscr = exml_query:attr(R2, <<"jid">>),
    io:format(" -- jid found : ~n~p", [JidOfSubscr]),
    JidOfSubscr =:= escalus_utils:get_jid(User).

subscribe_by_user(User, NodeName, NodeAddress) ->
    SubscribeToNode = pubsub_helper:create_subscribe_node_stanza(NodeName, User),
    UserName = escalus_utils:get_username(User),
    Id = <<UserName/binary,<<"binsuffix">>/binary>>,
    SubscribeToNodeIq  =  pubsub_helper:iq_with_id(set, Id, NodeAddress, User,  [SubscribeToNode]),
    io:format(" REQUEST SubscribeToNodeIq from user ~p: ~n~p~n",[UserName, SubscribeToNodeIq]),
    escalus:send(User, SubscribeToNodeIq),
    {true, RecvdStanza} = wait_for_stanza_and_match_result_iq(User, Id, NodeAddress), %%wait for subscr. confirmation
    io:format(" RESPONSE Subscriptions received by ~p: ~p~n",[UserName, RecvdStanza]),
    is_subscription_for_jid_pred(RecvdStanza, User, NodeName).

subscribe_by_users(UserList, NodeName, NodeAddress) ->
    lists:map(fun(User) -> subscribe_by_user(User, NodeName, NodeAddress) end, UserList).

%% the user unsubscribes himself
unsubscribe_by_user(User, NodeName, NodeAddress) ->
    UnubscribeFromNode = pubsub_helper:create_unsubscribe_from_node_stanza(NodeName, User),
    Id = <<"unsub1">>,
    UnSubscribeFromNodeIq  = pubsub_helper:iq_with_id(set, Id, NodeAddress, User,  [UnubscribeFromNode]),
    ct:pal(" REQUEST UnSubscribeFromNodeIq: ~n~n~p~n",[exml:to_binary(UnSubscribeFromNodeIq)]),
    escalus:send(User, UnSubscribeFromNodeIq),
    {true, _RecvdStanza} = pubsub_tools:wait_for_stanza_and_match_result_iq(User, Id, NodeAddress).

unsubscribe_by_users(UserList, NodeName, NodeAddress) ->
    lists:map(fun(User) -> unsubscribe_by_user(User, NodeName, NodeAddress) end, UserList).

%% returns value of 3rd level node attribute id, see XEP0060 example 100
get_publish_response_item_id(PublishItemConfirmation = #xmlel{name = <<"iq">>}) ->
    <<"result">> = exml_query:attr(PublishItemConfirmation, <<"type">>),
    L1 = exml_query:subelement(PublishItemConfirmation, <<"pubsub">>),
    L2 = exml_query:subelement(L1, <<"publish">>),
    L3 = exml_query:subelement(L2, <<"item">>),
    io:format(" item element: ~n~p~n", [L3]),
    exml_query:attr(L3, <<"id">>).
  
%% parameters: previously published item id returned by server
is_publish_response_matching_item_id(ItemTestId, PublishItemConfirmation) ->
    ItemTestId =:= get_publish_response_item_id(PublishItemConfirmation).

%% publish items witn contents specifying which sample content to use.
publish_sample_content(DestinationTopicName, DestinationNode, PublishItemId, User, SampleNumber) ->
    PublishToNode = case SampleNumber of
	sample_one -> pubsub_helper:create_publish_node_content_stanza(DestinationTopicName, PublishItemId);
	sample_two -> pubsub_helper:create_publish_node_content_stanza_second(DestinationTopicName, PublishItemId);
	sample_three -> pubsub_helper:create_publish_node_content_stanza_third(DestinationTopicName, PublishItemId);
	_ -> pubsub_helper:create_publish_node_content_stanza(DestinationTopicName, PublishItemId)
    end,
    
   IqId = <<"publish1">>,
   PublishToNodeIq  =  pubsub_helper:iq_with_id(set, IqId, DestinationNode, User,  [PublishToNode]),
   ReportString = " REQUEST PublishToNodeIq: ~n~p~n",
   ct:pal(ReportString, [exml:to_binary(PublishToNodeIq)]),
   io:format(ReportString, [PublishToNodeIq]),
   escalus:send(User, PublishToNodeIq),
   {true, _RecvdStanza} = wait_for_stanza_and_match_result_iq(User, IqId, DestinationNode).


%% extract the items from the nested wrapper "items" enclosed in message/event
%% according to example 101 of XEP there comes always only ONE item but we
%% deal with the lists anyway for consistency. 
get_event_notification_items_list(EventMessage = #xmlel{name = <<"message">>}) ->
    Event = exml_query:subelement(EventMessage, <<"event">>),
    ItemsWrapper = exml_query:subelement(Event, <<"items">>),
    Items = exml_query:subelements(ItemsWrapper, <<"item">>),
    Items.

%% according to example 194 , p 8.8.4
%% return information about current subscription state for a given user (e.g after
%% topic owner changed his subscription state.
get_event_notification_subscription_change(EventMessage = #xmlel{name = <<"message">>}) ->
    Event = exml_query:subelement(EventMessage, <<"event">>),
    true =  ?NS_PUBSUB_EVENT =:= exml_query:attr(Event, <<"xmlns">>),
    Subscription = exml_query:subelement(Event, <<"subscription">>),
    SubscrNode = exml_query:attr(Subscription, <<"node">>),
    SubscrJid = exml_query:attr(Subscription, <<"jid">>),
    SubscrStatus = exml_query:attr(Subscription, <<"subscription">>).

%% returns servers' response stanza, according to 8.8.1.1 (topic owner case!)
%% pass SubscrChangeData as List of tuples {jid, new_subscription_state}
request_subscription_changes_by_owner(User, NodeName, NodeAddr, SubscriptionChangeData) ->
    SubscriptionChangeDataStanza = pubsub_helper:get_subscription_change_list_stanza(SubscriptionChangeData),
    SetSubscriptionsStanza = pubsub_helper:set_subscriptions_stanza(NodeName, SubscriptionChangeDataStanza),
    Id = <<"subman2">>,
    SetSubscriptionsIq = pubsub_helper:iq_with_id(set, Id, NodeAddr, User, [SetSubscriptionsStanza]),
    io:format(" REQUEST ChangeSubscriptionsByByOwner: ~n~n~p~n",[SetSubscriptionsIq]),
    escalus:send(User, SetSubscriptionsIq ),
    wait_for_stanza_and_match_result_iq(User, Id, NodeAddr).

get_items_ids(ItemList) ->
    lists:map(fun(Item) -> exml_query:attr(Item, <<"id">>) end, ItemList).








