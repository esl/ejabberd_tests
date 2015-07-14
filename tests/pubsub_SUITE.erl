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
    [{pubsub, [sequence], [request_to_create_node, request_to_publish_to_node, request_to_subscribe_to_node]}].


suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config,{by_name, [alice]}).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config,{by_name, [alice]}).

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(_TestName, Config) ->
    escalus:init_per_testcase(_TestName, Config).

end_per_testcase(_TestName, Config) ->
    escalus:end_per_testcase(_TestName, Config).



%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------



%% ---------------- PUB SUB STANZAS -------------

pubsub_stanza(Children) ->
    #xmlel{name = <<"pubsub">>,
	     attrs = [{<<"xmlns">>, ?NS_PUBSUB} ],
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
    pubsub_stanza([PublNode]).


create_subscribe_node_stanza(NodeName, From) ->
    SubsrNode = #xmlel{name = <<"subscribe">>,
		      attrs = [
			       {<<"node">>, NodeName},
			       {<<"jid">>, escalus_utils:get_jid(From)}]
		      },
    pubsub_stanza([SubsrNode]).


%% ---END---------- PUB SUB STANZAS -------------
   
%% XEP0060---8.1.1 Create a node with default configuration ---------------------------
request_to_create_node(Config) ->

     escalus:story(Config, [{alice,1}],
		   fun(Alice) ->
			   PubSubCreate = create_specific_node_stanza(<<"princely_musings">>),
			   PubSub = pubsub_stanza([PubSubCreate]),
			   DestinationNode = <<"pubsub.localhost">>,
			   CreateNodeIq  =  iq_set_get(set, <<"create1">>, DestinationNode, Alice,  PubSub),
			   escalus:send(Alice, CreateNodeIq),
			   escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)) %% TODO : enhance this check (Example 131)
		   end).


%% XEP0060---7.1.1 Request to publish to a node -----------------------------------------
request_to_publish_to_node(Config) ->

     escalus:story(Config, [{alice,1}],
		   fun(Alice) ->
			   Publish = create_publish_node_content_stanza(<<"princely_musings">>),
			   DestinationNode = <<"pubsub.localhost">>,
			   PublishToNodeIq  =  iq_set_get(set, <<"publish1">>, DestinationNode, Alice,  Publish),
			   escalus:send(Alice, PublishToNodeIq),
			   escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)) %% TODO : enhance this check (Example 100)
			   %% Detect incoming event message for case with 1-Payload , 2-NoPayload
		   end).


    
%% XEP0060---6.1.1 Subscribe to node request --------------------------------------------
request_to_subscribe_to_node(Config) ->

     escalus:story(Config, [{alice,1}],
		   fun(Alice) ->
			   Subscribe = create_subscribe_node_stanza(<<"princely_musings">>, Alice),
			   DestinationNode = <<"pubsub.localhost">>,
			   SubscribeToNodeIq  =  iq_set_get(set, <<"sub1">>, DestinationNode, Alice,  Subscribe),
			   escalus:send(Alice, SubscribeToNodeIq),
			   escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)) %% TODO : enhance this check (Example 33)
			   %% Detect incoming event message for case with 1-Payload , 2-NoPayload
		   end).
















