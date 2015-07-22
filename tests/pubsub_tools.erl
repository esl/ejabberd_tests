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

-export([wait_for_stanza_and_match_result_iq/3,
	get_subscription_confirmation_stanza/1,
	get_publish_response_item_id/1,
	is_subscription_for_jid_pred/3,
	is_publish_response_matching_item_id/2,
	 publish_sample_content/5,
	subscribe_by_user/3]).

%% ----------------------------- HELPER and DIAGNOSTIC functions -----------------------
%% Note ------ functions in this section are not stanza generating functions but:
%% - predicates
%% - high level wrappers for escalus specific to pubsub feature

%% Checks superficialy is IQ from server  matches the sent id, there is "result" and sender is correct.
wait_for_stanza_and_match_result_iq(User, Id, DestinationNode) ->
    ResultStanza = escalus:wait_for_stanza(User),
    ReportString = "Response stanza from server: ~n~n",

    io:format(ReportString ++ "~p~n", [ResultStanza]),
    ct:pal(ReportString ++ "~s~n", [exml:to_binary(ResultStanza)]),

    QueryStanza = escalus_stanza:iq_with_type_id_from(<<"result">>, Id, DestinationNode),
    %%  ct:pal("QueryStanza: ~s~n",[exml:to_binary(QueryStanza)]),

    Result = escalus_pred:is_iq_result(QueryStanza, ResultStanza),
    %%  ct:pal(" result - ~s~n", [exml:to_binary(Result)]),

    {Result, ResultStanza}.

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
    ct:pal(" Request SubscribeToNodeIq from user ~p: ~n~p~n",[UserName, exml:to_binary(SubscribeToNodeIq)]),
    escalus:send(User, SubscribeToNodeIq),
    {true, RecvdStanza} = wait_for_stanza_and_match_result_iq(User, Id, NodeAddress), %%wait for subscr. confirmation
    ct:pal("Subscriptions received by ~p: ~s~n",[UserName, exml:to_binary(RecvdStanza)]),
    is_subscription_for_jid_pred(RecvdStanza, User, NodeName).

%% returns value of 3rd level node attribute id, see XEP0060 example 100
get_publish_response_item_id(PublishItemConfirmation) ->
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
	_ -> pubsub_helper:create_publish_node_content_stanza(DestinationTopicName, PublishItemId)
    end,
    
   IqId = <<"publish1">>,
   PublishToNodeIq  =  pubsub_helper:iq_with_id(set, IqId, DestinationNode, User,  [PublishToNode]),
   ReportString = " Request PublishToNodeIq: ~n~p~n",
   ct:pal(ReportString, [exml:to_binary(PublishToNodeIq)]),
   io:format(ReportString, [PublishToNodeIq]),
   escalus:send(User, PublishToNodeIq),
   {true, RecvdStanza} = wait_for_stanza_and_match_result_iq(User, IqId, DestinationNode).
