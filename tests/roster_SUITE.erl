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
    [
     {group, read}
    ,{group, write}
    ].

groups() ->
    [{read, [
	     user_asks_for_roster_after_log_in
	    ,user_asks_for_empty_roster
	    ,user_asks_for_trivial_nonempty_roster
	    ,user_asks_for_nontrivial_nonempty_roster
	    ,user_asks_for_contact_pending_out
	    ]},
     {write, [
	      user_adds_trivial_contact_to_empty_roster
	     ]}
    ].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config1 = escalus:init_per_suite(Config),
    Config2 = ejabberd_node_utils:init(Config1),
    ejabberd_node_utils:backup_config_file(Config2),
    ejabberd_node_utils:modify_config_file(http_backend_options(), Config2),
    ejabberd_node_utils:restart_application(ejabberd),
    Config2.

end_per_suite(Config) ->
    ejabberd_node_utils:restore_config_file(Config),
    ejabberd_node_utils:restart_application(ejabberd),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    %% GIVEN:
    escalus:create_users(Config, {by_name, [alice, bob]}).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, {by_name, [alice, bob]}).

init_per_testcase(TestCaseName, Config) ->
    escalus:init_per_testcase(TestCaseName, Config).

end_per_testcase(TestCaseName, Config) ->
    escalus:end_per_testcase(TestCaseName, Config).


%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

user_asks_for_roster_after_log_in(Config) ->
    %% GIVEN:
    http_roster_server:running(),

    %% WHEN:
    escalus:story(
      Config,
      [{alice, 1}],
      fun(_Alice) ->
	      ok
      end).

    %% THEN: ???

user_asks_for_empty_roster(Config) ->
    user_gets_roster_from_http_backend(Config, []).

user_asks_for_trivial_nonempty_roster(Config) ->
    BobJid = <<"bob@domain">>,
    Bob = {BobJid, [{<<"jid">>, BobJid}]},
    CarolJid = <<"carol@domain">>,
    Carol = {CarolJid ,[{<<"jid">>, CarolJid}]},
    Roster = [Bob, Carol],
    user_gets_roster_from_http_backend(Config, Roster).

user_asks_for_nontrivial_nonempty_roster(Config) ->
    BobJid = <<"bob@domain">>,
    Bob = {BobJid, [{<<"jid">>, BobJid},
		    {<<"name">>, <<"bob">>},
		    {<<"subscription">>, <<"to">>},
		    {<<"groups">>, [<<"wonderland">>,
				    <<"mathematics">>]}]},
    CarolJid = <<"carol@domain">>,
    Carol = {CarolJid ,[{<<"jid">>, CarolJid},
			{<<"name">>, <<"carol">>},
			{<<"subscription">>, <<"from">>}]},
    Roster = [Bob, Carol],
    user_gets_roster_from_http_backend(Config, Roster).

user_asks_for_contact_pending_out(Config) ->
    BobJid = <<"bob@domain">>,
    Bob = {BobJid,
	 [{<<"jid">>, BobJid},
	  {<<"name">>, <<"Bob">>},
	  {<<"subscription">>, <<"to">>},
	  {<<"ask">>, <<"subscribe">>},
	  {<<"groups">>,
	   [<<"Wonderland">>,
	    <<"Cryptography">>]}]},
    user_gets_roster_from_http_backend(Config, [Bob]).

user_adds_trivial_contact_to_empty_roster(Config) -> 
    %% GIVEN:
    http_roster_server:running(),
    
    %% WHEN:
    escalus:story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
	      %% GIVEN:
              user_has_external_roster(Alice, []),
	      BobJid = escalus_client:short_jid(Bob),
	      Proplist = [{<<"jid">>, BobJid},
			      {<<"name">>, <<"bobcat">>},
			      {<<"groups">>, [<<"wonderland">>,
					      <<"cryptography">>]}],
	      InputRoster = [{BobJid, Proplist}],
	      %% WHEN:
	      escalus:send(Alice, 
			   escalus_stanza:roster_add_contact(
			     Bob,
			     proplists:get_value(<<"groups">>, Proplist),
			     proplists:get_value(<<"name">>, Proplist))),
              %% Then:
	      OutputRoster = user_fetches_roster(Alice),
              rosters_equal(InputRoster, OutputRoster)
      end).


%% Auxilliary

user_gets_roster_from_http_backend(Config, InputRoster) ->
    %% GIVEN:
    http_roster_server:running(),

    escalus:story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
	      %% GIVEN: 
              user_has_external_roster(Alice, InputRoster),
	      %% WHEN:
              OutputRoster = user_fetches_roster(Alice),
              %% Then:
              rosters_equal(InputRoster, OutputRoster)
      end).


user_has_external_roster(User, Roster) ->
    UserJid = escalus_client:short_jid(User),
    http_roster_server:add_roster(UserJid, Roster).

user_fetches_roster(User) ->
    escalus:send(User, escalus_stanza:roster_get()),
    Result = escalus:wait_for_stanza(User),
    escalus_assert:is_roster_result(Result),
    get_roster_items(Result).

get_roster_items(Stanza) ->
    escalus:assert(is_iq_with_ns, [?NS_ROSTER], Stanza),
    Result = exml_query:subelement(Stanza, <<"query">>),
    Items = exml_query:paths(Result, [{element, <<"item">>}]),
    lists:map(fun ({xmlel, <<"item">>, Proplist, Children}) ->
		      Jid = proplists:get_value(<<"jid">>, Proplist),
		      case Children of
			  [] ->
			      {Jid, Proplist};
			  _ ->
			      Groups = extract_roster_groups(Children),
			      {Jid, [{<<"groups">>, Groups}|Proplist]}
		      end
	      end, Items).

roster_group({xmlel,<<"group">>,[],[{xmlcdata,Group}]}) ->
    Group.

extract_roster_groups(List) ->
    lists:map(fun roster_group/1, List).

rosters_equal(InputRoster, OutputRoster) ->
    lists:foreach(fun ({Jid, InputContactProplist}) ->
                          case proplists:get_value(Jid, OutputRoster) of
                              undefined ->
                                  error("Output roster does not contain user from the input");
                              OutputContactProplist ->
                                  compare_contacts(InputContactProplist, OutputContactProplist)
                          end
                  end,
                  InputRoster),
    true.

compare_contacts(InputContactProplist, OutputContactProplist) ->
    lists:foreach(fun ({Field, FieldValue}) ->
                          case proplists:get_value(Field, OutputContactProplist) of
                              undefined ->
                                  error("Field set in the contact is absent in the roster");
                              FieldValue ->
                                  ok;
                              _ ->
                                  error("Field has wrong value")
                          end
                  end, InputContactProplist).

http_backend_options() ->
    [{mod_roster, "{mod_roster, [{backend, http}]},"}].
