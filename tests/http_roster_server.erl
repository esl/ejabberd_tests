-module(http_roster_server).
-export([running/0, handle/3, add_roster/2]).
-define(MY_DATABASE, fake_roster_db).

running() ->
    application:ensure_all_started(axiom),
    axiom:start(?MODULE),
    ensure_table_running(),
    clean_all_rosters(),
    ok.

%% add_row/2
add_roster(User, Roster) ->
    ets:insert(?MY_DATABASE, {User, Roster}).

handle(<<"GET">>, [<<"roster">>, Domain, User], _Request) ->
    JID = jid(User, Domain),
    %% io:format("User = ~p, Domain = ~p, JID = ~p~n", [User, Domain, JID]),
    UserRoster = case ets:lookup(?MY_DATABASE, JID) of
		     [{_UserJID,Contacts}] ->
			 Contacts;
		     [] ->
			 []
		 end,
    roster_to_json(UserRoster).

%% record_to_JSON/1
roster_to_json(UserRoster) ->
    JsonUsers = lists:map(fun(Contact) ->
				  {struct, [{jid, Contact}]}
			  end,
			  UserRoster),
    JsonStruct = {struct, [{items, JsonUsers}]},
    list_to_binary(mochijson3:encode(JsonStruct)).

clean_all_rosters() ->
    true = ets:delete_all_objects(?MY_DATABASE).
    
ensure_table_running() ->
    case table_exists() of
        true -> ok;
        _ ->
            Parent = self(),
            spawn(fun() -> ets:new(?MY_DATABASE, [named_table, public]),
                           Parent ! initialized,
			   loop()
                  end),
            receive initialized -> ok
            after 500 -> error(db_not_ready) end
    end.

table_exists() ->
    case ets:info(?MY_DATABASE) of
        undefined ->
            false;
        _ ->
            true
    end.

loop() ->
    loop().

jid(U0, D0)
  when is_binary(U0) and is_binary(D0) ->
    U = binary_to_list(U0),
    D = binary_to_list(D0),
    list_to_binary(U++"@"++D).
