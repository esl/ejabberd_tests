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
    JID = escalus_utils:jid_to_lower(User),
    ets:insert(?MY_DATABASE, {JID, Roster}).

retrieve_roster(User) ->
    JID = escalus_utils:jid_to_lower(User),
    case ets:lookup(?MY_DATABASE, JID) of
	[{JID, Roster}] ->
	    Roster;
	[] ->
	    []
    end.

handle(<<"GET">>, [<<"roster">>, Domain, User], _Request) ->
    JID = jid(User, Domain),
    Items = retrieve_roster(JID),
    Result = items_to_json_roster(Items),
    Result;
handle(<<"PUT">>, [<<"roster">>, Domain, User, <<"contact">>], Request0) ->
    true = cowboy_req:has_body(Request0),
    {ok, Body, _Request1} = cowboy_req:body(Request0),
    JID = jid(User, Domain),
    Contact = from_json_to_contact_proplist(Body),
    ets:insert(?MY_DATABASE, {JID, Contact}),
    Body.

items_to_json_roster(Items) ->
    JsonStructItems = lists:map(fun json_item_struct/1, Items),
    RosterStruct = json_roster_struct(<<"0">>, JsonStructItems),
    Result = mochijson3:encode(RosterStruct),
    Result.

json_item_struct({_Jid, Proplist}) ->
    {struct,
     Proplist}.

from_json_to_contact_proplist(JSON) ->
    Struct = mochijson3:decode(JSON),
    {struct, [{<<"item">>, Proplist}]} = Struct,
    Proplist.

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
