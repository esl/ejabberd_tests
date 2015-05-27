%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
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

-module(roster_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, essential}].

groups() ->
    [{essential, [user_gets_roster_from_http_backend]}].

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
    escalus:create_users(Config, {by_name, [alice, bob]}).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, {by_name, [alice, bob]}).

init_per_testcase(TestCaseName, Config) ->
    escalus:init_per_testcase(TestCaseName, Config).

end_per_testcase(TestCaseName, Config) ->
    escalus:end_per_testcase(TestCaseName, Config).


user_gets_roster_from_http_backend(Config) ->
    escalus:story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              %% GIVEN:
              %% user_exists(alice),
              %% user_logged_in(alice),
              http_roster_server:running(),
              user_has_external_roster(Alice, []),
              %% When:
              Roster = user_fetches_roster(Alice),
              %% Then:
              [] = Roster
      end).


user_has_external_roster(User, Roster) ->
  
    UserJid = escalus_client:short_jid(User),
    http_roster_server:add_roster(UserJid, Roster).

user_fetches_roster(User) ->
    escalus:send(User, escalus_stanza:roster_get()),
    Result = escalus:wait_for_stanza(User),
    escalus_assert:is_roster_result(Result),
    get_roster_items(Result).

-spec get_roster_items(xmlterm()) -> [xmlterm()].
get_roster_items(Stanza) ->
    escalus:assert(is_iq_with_ns, [?NS_ROSTER], Stanza),
    Query = exml_query:subelement(Stanza, <<"query">>),
    Query#xmlel.children.
