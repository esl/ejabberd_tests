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
     {group, node_lifecycle},
     {group, pep}].

groups() ->
    [{disco, [], [pubsub_feature,
                  has_node]},
     %% These are run in sequence for a reason.
     %% Each one sets up the env in a particular state expected by the next.
     %% On deregistering the node owner all his nodes are deleted,
     %% so don't expect to find any changes in `pubsub_node' ets
     %% after the group has finished.
     {node_lifecycle, [sequence], [create_test,
                                   subscribe_test,
                                   publish_test]},
     {pep, [], [pep_publish]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(pep, Config) ->
    escalus:create_users(Config, {by_name, [alice, bob]});
init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(pep, Config) ->
    escalus:delete_users(Config, {by_name, [alice, bob]});
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

%% `pep_publish/1' relies on PubSub PEP profile auto-create,
%% auto-subscribe and filtered-notifications features.
pep_publish(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        escalus:send(Bob, escalus_stanza:presence_direct(Alice,
                                                         <<"subscribe">>)),
        escalus:assert(is_presence_with_type, [<<"subscribe">>],
                       escalus:wait_for_stanza(Alice)),
        escalus:send(Alice, escalus_stanza:presence_direct(Bob,
                                                           <<"subscribed">>)),
        %% Discard ask=subscribe (subscription=none) roster push
        escalus:assert(is_roster_set, escalus:wait_for_stanza(Bob)),
        %% Discard subscription-to roster push
        escalus:assert(is_roster_set, escalus:wait_for_stanza(Bob)),
        escalus:assert(is_presence_with_type, [<<"subscribed">>],
                       escalus:wait_for_stanza(Bob)),
        %% Discard subscription from bob@localhost roster push
        escalus:assert(is_roster_set, escalus:wait_for_stanza(Alice)),
        print_c2s_state(Alice),
        print_c2s_state(Bob),
        %% Send a fake caps presence to make the server ask about
        %% Bob's identity+features.
        escalus:send(Bob, fake_caps_presence(<<"wonderland">>)),
        %% Ensure the server sends expected disco#info.
        DiscoInfo = escalus:wait_for_stanza(Bob),
        escalus:assert(is_disco_info, DiscoInfo),
        %% Send the identity+features.
        escalus:send(Bob, pc_client_info(DiscoInfo,
                                         [<<"wonderland">>,
                                          <<"wonderland+notify">>])),
        %% Discard Bob's broadcasted presence.
        escalus:assert(is_presence_with_type, [<<"available">>],
                      escalus:wait_for_stanza(Bob)),
        print_c2s_state(Alice),
        print_c2s_state(Bob),
        %% Publish an event.
        Publish = escalus_stanza:publish(<<"wonderland">>,
                                         wonderful_items()),
        escalus:send(Alice, Publish),
        escalus:assert(is_publish_result, [<<"wonderland">>],
                       escalus:wait_for_stanza(Alice)),
        escalus:wait_for_stanzas(Bob, 10),
        ct:fail(unfinished)
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

fake_caps_presence(Node) ->
    escalus_stanza:presence(<<"available">>, [fake_caps(Node)]).

fake_caps(Node) ->
    #xmlel{name = <<"c">>,
           attrs = [{<<"xmlns">>, ?NS_CAPS},
                    {<<"node">>, Node},
                    %% XXX This is the lame fake part.
                    %%     The value should be computed based on the entity
                    %%     capabilities but that's a really involved process.
                    {<<"ver">>, id()}]}.

pc_client_info(DiscoInfo, Features) ->
    Q = #xmlel{name = <<"query">>,
               attrs = [{<<"xmlns">>, ?NS_DISCO_INFO}],
               children = [pc_client_identity()] ++ [feature(F)
                                                     || F <- Features]},
    (escalus_stanza:iq_result(DiscoInfo))#xmlel{children = [Q]}.

pc_client_identity() ->
    #xmlel{name = <<"identity">>,
           attrs = [{<<"category">>, <<"client">>},
                    {<<"type">>, <<"pc">>}]}.

feature(F) ->
    #xmlel{name = <<"feature">>,
           attrs = [{<<"var">>, F}]}.

-spec id() -> binary().
id() ->
    %% A static ID here makes the server recognize this set of capabilities.
    %% In other words, it triggers mod_pubsub:caps_update with a sensible
    %% set of advertised features.
    %% Why doesn't a disco#info reply trigger that call?
    %% Maybe the node value of "wonderland#123456" (where 123456 is the id
    %% sent to the server) must be copied inside the disco#info reply?
    <<"11111">>.

print_c2s_state(#client{jid = Jid}) ->
    [U,S,R] = [binary_to_list(E)
               || E <- binary:split(Jid, [<<"@">>, <<"/">>], [global])],
    Pid = escalus_ejabberd:rpc(ejabberd_sm, get_session_pid, [U,S,R]),
    State = sys:get_status(Pid),
    error_logger:info_msg("~s: ~p~n", [Jid, State]).
