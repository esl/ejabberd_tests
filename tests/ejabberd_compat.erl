-module(ejabberd_compat).

-compile([export_all]).

get_remote_sessions() ->
    escalus_ejabberd:rpc(ejabberd_sm, dirty_get_sessions_list, []).
