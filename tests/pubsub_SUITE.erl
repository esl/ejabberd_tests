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

all() ->
    [{group, pubsub}].

groups() ->
%%    [{pubsub, [sequence], [request_to_create_node, request_to_publish_to_node, request_to_subscribe_to_node]}].
%%    [{pubsub, [sequence], [request_to_create_node]}].
    [{pubsub, [sequence], [request_to_create_node, request_to_delete_node]}].



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
    escalus:create_users(Config,{by_name, [alice]}).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config,{by_name, [alice]}).

init_per_testcase(_TestName, Config) ->
    escalus:init_per_testcase(_TestName, Config).

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

iq_set_get(set, Id, To, From,  Body) ->
    S1 = escalus_stanza:iq(To, <<"set">>, Body),
    iq_set_get_rest(S1, Id, From);
    

iq_set_get(get, Id, To, From,  Body) ->
    S1 = escalus_stnanza:iq(To, <<"get">>, Body),
    iq_set_get_rest(S1, Id, From).

iq_set_get_rest(PrevPart, Id, From) ->
    S2 = escalus_stanza:set_id(PrevPart, Id),							
    escalus_stanza:from(S2, escalus_utils:get_jid(From)).


sample_publish_item_body() ->
    #xmlel{
       name = <<"entry">>,
       attrs = [{<<"xmlns">>, <<"http://www.w3.org/2005/Atom">>}],
       children = []
      }.

sample_publish_item(Id) ->
    #xmlel{
       name = <<"item">>,
       attrs = [{<<"id">>, Id}],
       children = sample_publish_item_body()
      }.

sample_publish_node_with_content(NodeName) ->
    #xmlel{
       name = <<"publish">>,
       attrs = [{<<"node">>, NodeName}],
       children = sample_publish_item(<<"abc123">>)
      }.
  

create_publish_node_content_stanza(NodeName) ->
    PublNode = sample_publish_node_with_content(NodeName),
    pubsub_stanza([PublNode], ?NS_PUBSUB_OWNER).


create_subscribe_node_stanza(NodeName, From) ->
    SubsrNode = #xmlel{name = <<"subscribe">>,
		      attrs = [
			       {<<"node">>, NodeName},
			       {<<"jid">>, escalus_utils:get_jid(From)}]
		      },
    pubsub_stanza([SubsrNode], ?NS_PUBSUB).

delete_node_stanza(NodeName) ->
    DelNode = #xmlel{name = <<"delete">>,
		      attrs = [
			       {<<"node">>, NodeName}
			      ]
		      },
    pubsub_stanza([DelNode], ?NS_PUBSUB_OWNER).

%% -----------------------------  dummy test ---------------- del me --------



available(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        escalus:send(Alice, escalus_stanza:presence(<<"available">>)),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice))

        end).


%% ---END---------- PUB SUB STANZAS -------------

-define (DEST_NODE, <<"pubsub.localhost">>).
-define (DEFAULT_TOPIC, <<"princely_musings">>).
   
%% XEP0060---8.1.1 Create a node with default configuration ---------------------------
request_to_create_node(Config) ->
    escalus:story(Config, [1],
		   fun(Alice) ->
			   PubSubCreate = create_specific_node_stanza(?DEFAULT_TOPIC),
			   PubSub = pubsub_stanza([PubSubCreate], ?NS_PUBSUB),
			   DestinationNode = ?DEST_NODE,
			   PubSubCreateIq  =  iq_set_get(set, <<"create1">>, DestinationNode, Alice,  PubSub),
			   io:format("PubSubCreateIq: ~p~n",[PubSubCreateIq]),
			   escalus:send(Alice, PubSubCreateIq),
			   %%escalus:wait_for_stanzas(Alice, 3)
			   escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)) %% TODO : enhance this check (Example 131)
		   end).


%% XEP0060---7.1.1 Request to publish to a node -----------------------------------------
request_to_publish_to_node(Config) ->
     escalus:story(Config, [1],
		   fun(Alice) ->
			   PublishToNode = create_publish_node_content_stanza(?DEFAULT_TOPIC),
			   DestinationNode = ?DEST_NODE,
			   PublishToNodeIq  =  iq_set_get(set, <<"publish1">>, DestinationNode, Alice,  PublishToNode),
			   escalus:send(Alice, PublishToNodeIq),
			   escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice, 5)) %% TODO : enhance this check (Example 100)
			   %% Detect incoming event message for case with 1-Payload , 2-NoPayload
		   end).

    
%% XEP0060---6.1.1 Subscribe to node request --------------------------------------------
request_to_subscribe_to_node(Config) ->

     escalus:story(Config, [1],
		   fun(Alice) ->
			   SubscribeToNode = create_subscribe_node_stanza(?DEFAULT_TOPIC, Alice),
			   DestinationNode = ?DEST_NODE,
			   SubscribeToNodeIq  =  iq_set_get(set, <<"sub1">>, DestinationNode, Alice,  SubscribeToNode),
			   escalus:send(Alice, SubscribeToNodeIq),
			   escalus:wait_for_stanzas(Alice,3)
			   %%RecvedStanza = escalus:wait_for_stanza(Alice),
			   %%escalus:assert(is_iq_result, RecvedStanza) %% TODO : enhance this check (Example 33)
			   %% Detect incoming event message for case with 1-Payload , 2-NoPayload
		   end).


%% XEP0060---8.4.1 Delete node request --------------------------------------------
request_to_delete_node(Config) ->

     escalus:story(Config, [1], 
		   fun(Alice) ->
			   DeleteNode = delete_node_stanza(?DEFAULT_TOPIC),
			   DestinationNode = ?DEST_NODE,
			   DeleteNodeIq  =  iq_set_get(set, <<"delete1">>, DestinationNode, Alice,  DeleteNode),

			   io:format("DeleteNodeIqToSend: ~p~n",[DeleteNodeIq]),
			   escalus:send(Alice, DeleteNodeIq),

			   ResultStanza = escalus:wait_for_stanza(Alice),
			   io:format("ResultStanza: ~p~n",[ResultStanza]),

			   %%QueryStanza = escalus_stanza:iq_with_type_id_from(<<"result">>, <<"delete1">>, DestinationNode),
			   %%io:format("QueryStanza: ~p~n",[QueryStanza]),

			   escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)) %% TODO : enhance this check (Example 131)
			   
%%			   escalus:assert(is_iq_result, QueryStanza, ResultStanza) %% enhance this check (Example 157)
		   end).















