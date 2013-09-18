-module(pubsub_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
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
                                   subscribe_test,
                                   publish_test]}].

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
    escalus:story(Config, [{bob,1}], fun(Bob) ->
        BareBob = escalus_utils:get_short_jid(Bob),
        Subscribe = escalus_stanza:subscribe(?PUBSUB_JID, <<"wonderland">>,
                                             BareBob),
        escalus:send(Bob, Subscribe),
        escalus:assert(is_pubsub_event, escalus:wait_for_stanza(Bob)),
        escalus:assert(is_subscription, [<<"wonderland">>, BareBob],
                       escalus:wait_for_stanza(Bob))
    end).

publish_test(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        Publish = escalus_stanza:publish(<<"wonderland">>,
                                         wonderful_items()),
        escalus:send(Alice, Publish),
        escalus:assert(is_publish_result, [<<"wonderland">>],
                       escalus:wait_for_stanza(Alice)),
        escalus:assert(is_pubsub_event, escalus:wait_for_stanza(Bob))
    end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

wonderful_items() ->
    [#xmlel{name = <<"item">>,
            children = [entry()]}].

entry() ->
    C = <<"Beware the Jabberwock, my son,\n"
          "the jaws that bite and claws that scratch\n"
          "Beware the jubjub bird\n"
          "and shun the frumious bandersnatch.\n">>,
    #xmlel{name = <<"entry">>,
           children = [#xmlcdata{content = C}]}.
