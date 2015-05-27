-module(http_roster_server).
-export([running/0, handle/3, add_roster/2]).
-define(MY_DATABASE, fake_roster_db).

running() ->
    application:ensure_all_started(axiom),
    axiom:start(?MODULE),
    ensure_table_running(),
    clean_all_rosters(),
    ok.

add_roster(User, Roster) ->
    ets:insert(?MY_DATABASE, {User, Roster}).

handle(<<"GET">>, [<<"roster">>, Jid], _Request) ->
    UserRoster = ets:lookup(?MY_DATABASE, Jid),
    %% TODO return json for this usee
    %% roster_to_json(UserRoster).
    roster_to_json(UserRoster).

roster_to_json(UserRoster) ->
    JsonUsers = lists:map(fun(Contact) ->
                                  {struct, [{jid, Contact}]}
                          end,
                          UserRoster),
    JsonStruct = {struct, [{items, JsonUsers}]},
    mochijson3:encode(JsonStruct).

clean_all_rosters() ->
    true = ets:delete_all_objects(?MY_DATABASE).
    
ensure_table_running() ->
    case table_exists() of
        true -> ok;
        _ ->
            Parent = self(),
            spawn(fun() -> ets:new(?MY_DATABASE, [named_table, public]),
                           Parent ! initialized,
                           %% TODO run me in a looping child process
                           timer:sleep(60*1000)
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
