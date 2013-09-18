-module(pubsub_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

-define(PUBSUB, <<"http://jabber.org/protocol/pubsub">>).
-define(PUBSUB_JID, <<"pubsub.localhost">>).

all() ->
    [{group, disco},
     {group, node_lifecycle}].

groups() ->
    [{disco, [], [pubsub_feature,
                  has_node]},
     {node_lifecycle, [sequence], [create_test,
                                   subscribe_test]}].

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
    escalus:create_users(Config).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------

pubsub_feature(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        escalus:send(Alice, escalus_stanza:disco_info(?PUBSUB_JID)),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Stanza),
        escalus:assert(has_feature, [?PUBSUB], Stanza)
    end).

%% I don't know yet why this node exists, but just want to have
%% some rough but automatic verification that mod_pubsub works.
has_node(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        escalus:send(Alice, escalus_stanza:disco_items(?PUBSUB_JID)),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Stanza),
        escalus:assert(has_item, [?PUBSUB_JID, <<"/home">>], Stanza)
    end).

create_test(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        Create = escalus_stanza:create_node(?PUBSUB_JID, <<"wonderland">>),
        escalus:send(Alice, Create),
        escalus:assert(is_node_created, [<<"wonderland">>],
                       escalus:wait_for_stanza(Alice))
    end).

subscribe_test(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        BareAlice = escalus_utils:get_short_jid(Alice),
        Subscribe = escalus_stanza:subscribe(?PUBSUB_JID, <<"wonderland">>,
                                             BareAlice),
        escalus:send(Alice, Subscribe),
        escalus:assert(is_pubsub_event, escalus:wait_for_stanza(Alice)),
        escalus:assert(is_subscription, [<<"wonderland">>, BareAlice],
                       escalus:wait_for_stanza(Alice))
    end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

publish_reach(From, Addrs) ->
    Reach = escalus_stanza:reach(Addrs),
    Items = escalus_stanza:pubsub_items([Reach]),
    escalus_stanza:publish_iq(From, ?NS_REACH, Items).
