-module(dynamic_modules).

-export([stop/2, start/3, restart/3]).


stop(Domain, Mod) ->
    io:format("stopping ~p", [Mod]),
    case escalus_ejabberd:rpc(code, which, [Mod]) of
        non_existing ->
            ok;
        _ ->
            {atomic, ok} = escalus_ejabberd:rpc(gen_mod, stop_module, [Domain, Mod]),
            ok
    end.

start(Domain, Mod, Args) ->
    io:format("starting ~p", [Mod]),
    case escalus_ejabberd:rpc(gen_mod, start_module, [Domain, Mod, Args]) of
        {badrpc, Reason} ->
            ct:fail("Cannot start module ~p reason ~p", [Mod, Reason]);
        _ -> ok
    end.

restart(Domain, Mod, Args) ->
    ModStr = case lists:reverse(atom_to_list(Mod)) of
        "cbdo_" ++ Rest ->
            lists:reverse(Rest);
        "knab_" ++ Rest ->
            lists:reverse(Rest);
        Other ->
            lists:reverse(Other)
    end,
    _ = [stop(Domain, list_to_atom(M)) ||
         M <- [ModStr, ModStr ++ "_odbc", ModStr ++ "_bank"]],
    start(Domain, Mod, Args).
