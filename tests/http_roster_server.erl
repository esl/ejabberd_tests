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
    ets:insert(?MY_DATABASE, {escalus_utils:jid_to_lower(User), Roster}).

retrieve_roster(User) ->
    [{User, Roster}] = ets:lookup(?MY_DATABASE, escalus_utils:jid_to_lower(User)),
    Roster.

handle(<<"GET">>, [<<"roster">>, Domain, User], _Request) ->
    ?debugFmt("USER ~p ~n", [User]),
    JID = jid(User, Domain),
    Items = retrieve_roster(JID),
    Result = items_to_json_roster(Items),
    ?debugFmt("FINAL RESULT ~p~n", [Result]),
    Result.

items_to_json_roster(Items) ->
    JsonStructItems = lists:map(fun json_item_struct/1, Items),
    RosterStruct = json_roster_struct(<<"0">>, JsonStructItems),
    Result = mochijson3:encode(RosterStruct),
    Result.

json_item_struct({_Jid, Proplist}) ->
    {struct,
     Proplist}.

json_roster_struct(Ver, JsonStructItems) ->
    {struct,
     [{version, Ver},
      {items, JsonStructItems}]}.

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
    receive
	stop -> ok;
	_ -> loop()
    end.

jid(User, Domain) ->
    <<User/binary, "@", Domain/binary>>.
