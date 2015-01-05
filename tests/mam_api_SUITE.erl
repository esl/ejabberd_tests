%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(mam_api_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-import(ejabberd_node_utils, [call_fun/3]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
all() ->
    [{group, positive}].

groups() ->
    [{positive, [], [simple_query]}].

init_per_suite(Config) ->
    Config1 = escalus:init_per_suite(Config),
    katt_helper:init_per_suite(Config1).

end_per_suite(Config) ->
    katt_helper:end_per_suite(Config),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, {by_name, [alice, bob]}).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, {by_name, [alice, bob]}).

init_per_testcase(CaseName, Config) ->
    Config1 = clean_archives([alice, bob], Config),
    escalus:init_per_testcase(CaseName, Config1).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% simple mam query with all messages and default limits
%%--------------------------------------------------------------------
simple_query(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi there!">>)),
        escalus:wait_for_stanza(Bob),
        escalus:send(Bob, escalus_stanza:chat_to(Alice, <<"Oh hai!">>)),
        escalus:wait_for_stanza(Alice),

        Params = [{host, ct:get_config(ejabberd_domain)},
                  {username, escalus_client:username(Alice)}],
        Result = katt_helper:run(simple_archive, Config, Params),

        verify_messages(Result, [{lower_jid(Alice), jid(Bob), "Hi there!"},
                                 {lower_jid(Bob), jid(Alice), "Oh hai!"}]),
        ok
    end).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
clean_archives(Users, Config) ->
    ServerUsers = [{escalus_users:get_server(Config, User),
                    escalus_users:get_username(Config, User)}
                   || User <- Users],
    [ok = delete_archive(S, U) || {S, U} <- ServerUsers],
    [0 = archive_size(S, U) || {S, U} <- ServerUsers],
    Config.

delete_archive(Server, Username) ->
    call_fun(mod_mam, delete_archive, [Server, Username]).

archive_size(Server, Username) ->
    call_fun(mod_mam, archive_size, [Server, Username]).

all_responses({_, _, _, _, Results}) ->
    [Response || {_, _, _, {katt_response, _, _, _, Response}, _} <- Results].

verify_messages(Result, Stanzas) ->
    Responses = lists:flatten(all_responses(Result)),
    true = do_verify_messages(Responses, Stanzas).

do_verify_messages(_, []) ->
    true;
do_verify_messages([], _) ->
    false;
do_verify_messages(Responses, [Stanza|Rest]) ->
    case does_contain_message(Responses, Stanza) of
        false -> 
            false;
        {true, MatchedStanza} ->
            do_verify_messages(Responses -- [MatchedStanza], Rest)
    end. 

does_contain_message([], _Stanza) ->
    false;
does_contain_message([Candidate|Rest], {From, To, Content}=Stanza) ->
    MaybeFrom = proplist_path(["stanza", "jid"], Candidate),
    MaybeTo = proplist_path(["stanza", "payload", "message", "to"], Candidate),
    [Body] = proplist_path(["stanza", "payload", "message", "children"],
                           Candidate),
    [MaybeContent] = proplist_path(["body", "children"], Body),
    case MaybeFrom =:= From andalso
         MaybeTo =:= To andalso
         MaybeContent =:= Content of
        true ->
            {true, Candidate};
        _ ->
            does_contain_message(Rest, Stanza)
    end.

proplist_path(Path, Proplist) ->
    proplist_path(Path, Proplist, undefined).

proplist_path([], Value, _Default) ->
    Value;
proplist_path([Key|Rest], {Key, Value}, Default) ->
    proplist_path(Rest, Value, Default);
proplist_path([Key|Rest], Proplist, Default) when is_list(Proplist) ->
    case lists:keyfind(Key, 1, Proplist) of
        {Key, Value} ->
            proplist_path(Rest, Value, Default);
        false ->
            Default
    end;
proplist_path(_, _, Default) ->
    Default.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
jid(Client) ->
    Jid = escalus_client:full_jid(Client),
    binary_to_list(Jid).

lower_jid(Client) ->
    string:to_lower(jid(Client)).
