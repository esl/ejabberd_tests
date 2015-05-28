-module(http_roster_server).
-export([running/0, handle/3, add_roster/2]).
-define(MY_DATABASE, fake_roster_db).
-include_lib("../../../apps/ejabberd/include/mod_roster.hr").

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
    Items = case ets:lookup(?MY_DATABASE, {User, Domain, JID}) of
		     Rows ->
			 Rows;
		     [] ->
			 []
		 end,
    rows_to_json_roster(Items).

%% record_to_JSON/1
rows_to_json_roster(Items) ->    
    ItemsStruct = lists:map(fun json_item_struct/1, Items),
    %% TODO: version numbering
    RosterStruct = json_roster_struct(<<"0">>, ItemsStruct),
    list_to_binary(mochijson3:encode(RosterStruct)).


json_item_struct(#record{jid = JID,
			 name = Name,
			 subscription = Sub,
			 ask = Ask,
			 group = Groups}) ->
    {struct,
     [{jid, JID},
      {name, Name},
      {subscription, Sub},
      {ask, Ask},
      {approved, none},
      {group, Groups}]}.

json_roster_struct(Ver, Items) ->
    {struct,
     [{version, Ver},
      {items, Items}]}.


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
