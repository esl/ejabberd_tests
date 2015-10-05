%%==============================================================================
%% Copyright 2012 Erlang Solutions Ltd.
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

-module(oauth_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, token_login},
     {group, token_revocation},
     {group, commands}
    ].

groups() ->
    [
     {token_login, [sequence], token_login_tests()},
     {token_revocation, [sequence], token_revocation_tests()},
     {commands, [], [revoke_token_cmd_when_no_token]}
    ].

token_login_tests() ->
    [
     request_tokens_test,
     login_access_token_test,
     login_refresh_token_test
    ].

token_revocation_tests() ->
    [
     %% TODO: isolate this test case - clean up the db before it!
     login_with_revoked_token_test,
     token_revocation_test
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(GroupName, Config0) ->
    Config = case GroupName of
                 commands -> ejabberd_node_utils:init(Config0);
                 _ -> Config0
             end,
    case get_auth_method() of
        external ->
            {skip, "external authentication requires plain password"};
        _ ->
            config_password_format(GroupName),
            Config2 = escalus:create_users(Config, {by_name, [john, alice]}),
            %% ct:pal("-- created users ~p ", [Config2]),
            assert_password_format(GroupName, Config2)
    end.

end_per_group(_GroupName, Config) ->
    set_store_password(plain),
    escalus:delete_users(Config, {by_name, [john, alice]}).

init_per_testcase(CaseName, Config0) ->
    clean_token_db(),
    escalus:init_per_testcase(CaseName, Config0).

end_per_testcase(CaseName, Config) ->
    clean_token_db(),
    escalus:end_per_testcase(CaseName, Config).

%%
%% Tests
%%

request_tokens_test(Config) ->
    request_tokens_once_logged_in_impl(Config, john).

login_with_revoked_token_test(Config) ->
    %% given
    RevokedToken = get_revoked_token(Config, john),
    %ct:pal("serialized: ~p", [RevokedToken]),
    login_failure_with_revoked_token(Config, john, RevokedToken).

login_failure_with_revoked_token(Config, User, Token) ->
    %% when
    Result = login_with_token(Config, User, Token),
    % then
    {{auth_failed, _}, _, _} = Result.

get_revoked_token(Config, UserName) ->
    BJID = escalus_users:get_jid(Config, UserName),
    JID = escalus_ejabberd:rpc(jlib, binary_to_jid, [BJID]),
    Token = escalus_ejabberd:rpc(mod_auth_token, token, [refresh, JID]),
    ValidSeqNo = escalus_ejabberd:rpc(mod_auth_token_backend, get_valid_sequence_number,
                                      [JID]),
    %ct:pal( "~n ValidSEQ ~n~p", [ValidSeqNo]),
    RevokedToken0 = record_set(Token, [{5, invalid_sequence_no(ValidSeqNo)},
                                       {6, undefined},
                                       {7, undefined}]),
    RevokedToken = escalus_ejabberd:rpc(mod_auth_token, token_with_mac, [RevokedToken0]),
    %ct:pal("revoked: ~p", [RevokedToken]),
    escalus_ejabberd:rpc(mod_auth_token, serialize, [RevokedToken]).

invalid_sequence_no(SeqNo) ->
    SeqNo - 1.

request_tokens_once_logged_in(Config) ->
    request_tokens_once_logged_in_impl(Config, john).

request_tokens_once_logged_in_impl(Config, User) ->
    Self = self(),
    Ref = make_ref(),
    escalus:story(Config, [{User, 1}], fun(Client) ->
                                               ClientShortJid = escalus_utils:get_short_jid(Client),
                                               R = escalus_stanza:query_el(?NS_AUTH_TOKEN, []),
                                               IQ = escalus_stanza:iq(ClientShortJid, <<"get">>, R),
                                                % ct:pal("--test IQ w query ~p " , [IQ]),
                                               escalus:send(Client, IQ),
                                               Result = escalus:wait_for_stanza(Client),
                                               {AT, RT} = extract_tokens(Result),
                                               Self ! {tokens, Ref, {AT,RT}},
                                               ct:pal("~n Access token: ~n ~p ~nRefresh token: ~n ~p ~n " , [AT,RT])
                                       end),
    receive
        {tokens, Ref, Tokens} ->
            Tokens
    after
        1000 -> error
    end.

login_access_token_test(Config) ->
    Tokens = request_tokens_once_logged_in_impl(Config, john),
    login_access_token_impl(Config, Tokens).

login_refresh_token_test(Config) ->
    Tokens = request_tokens_once_logged_in_impl(Config, john),
    login_refresh_token_impl(Config, Tokens).

login_refresh_token_impl(Config, {_AccessToken, RefreshToken}) ->
    JohnSpec = escalus_users:get_userspec(Config, john),

    ConnSteps = [start_stream,
                 stream_features,
                 maybe_use_ssl,
                 maybe_use_compression
                ],

    {ok, ClientConnection, Props, _Features} = escalus_connection:start(JohnSpec, ConnSteps),

    ct:pal( " ~n ------ refresh token sent~n ~p ~n ", [RefreshToken]),

    Props2 = lists:keystore(oauth_token, 1, Props, {oauth_token, RefreshToken}),

    AuthResultToken = (catch escalus_auth:auth_sasl_oauth(ClientConnection, Props2)),
    ct:pal( " ~n ------ access token received~n ~p ~n ", [AuthResultToken]),
    ok.

%% users logs in using access token he obtained in previous session (stream has been
%% already reset)
login_access_token_impl(Config, {AccessToken, _RefreshToken}) ->
    {{ok, _ }, ClientConnection, Props2} = login_with_token(Config, john, AccessToken),
    escalus_connection:reset_parser(ClientConnection),
    {Props3, []} = escalus_session:start_stream(ClientConnection, Props2),
    NewFeatures = escalus_session:stream_features(ClientConnection, Props3, []),
    %todo: create step out of above lines
    {NewClientConnection, Props4, NewFeatures2} =
        escalus_session:bind(ClientConnection, Props3, NewFeatures),
    {NewClientConnection2, _Props5, _NewFeatures3} =
        escalus_session:session(NewClientConnection, Props4, NewFeatures2),
    escalus:send(NewClientConnection2, escalus_stanza:presence(<<"available">>)),
    escalus:assert(is_presence, escalus:wait_for_stanza(NewClientConnection2)).

login_with_token(Config, User, Token) ->
    UserSpec = escalus_users:get_userspec(Config, User),
    ConnSteps = [start_stream,
                        stream_features,
                        maybe_use_ssl,
                        maybe_use_compression
                        ],
    {ok, ClientConnection, Props, _Features} = escalus_connection:start(UserSpec, ConnSteps),
    %ct:pal( " ~n ------connection data ~p ~n ", [ClientConnection]),
    %ct:pal( " ~n ------Stream Features [0]   ~p ~n ", [Features]),
    Props2 = lists:keystore(oauth_token, 1, Props, {oauth_token, Token}),
    AuthResult = (catch escalus_auth:auth_sasl_oauth(ClientConnection, Props2)),
    %ct:pal( " ~n ------ SASL (access token) auth result   ~p ~n ", [AuthResult]),
    {AuthResult, ClientConnection, Props2}.

token_revocation_test(Config) ->
    %% given
    {Owner, _SeqNoToRevoke, Token} = get_owner_seqno_to_revoke(Config, john),
    %% when
    ok = revoke_token(Owner),
    %% then
    login_failure_with_revoked_token(Config, john, Token).

get_owner_seqno_to_revoke(Config, User) ->
    {_, RefreshToken} = request_tokens_once_logged_in_impl(Config, User),
    [_, BOwner, _, SeqNo, _] = binary:split(RefreshToken, <<0>>, [global]),
    Owner = escalus_ejabberd:rpc(jlib, binary_to_jid, [BOwner]),
    {Owner, binary_to_integer(SeqNo), RefreshToken}.

revoke_token(Owner) ->
    Result =  escalus_ejabberd:rpc(mod_auth_token, revoke, [Owner]),
    ct:pal("~n revoke result ~p ", [Result]),
    Result.

revoke_token_cmd_when_no_token(Config) ->
    %% given existing user with no token
    %% when revoking token
    R = mimctl(Config, ["revoke_token", escalus_users:get_jid(Config, john)]),
    %% then no token was found
    %ct:pal("~p", [R]),
    "User or token not found.\n" = R.

%%
%% Helpers
%%

extract_tokens(#xmlel{name = <<"iq">>, children = [#xmlel{name = <<"items">>} = Items ]}) ->
    ATD = exml_query:path(Items, [{element, <<"access_token">>}, cdata]),
    RTD = exml_query:path(Items, [{element, <<"refresh_token">>}, cdata]),
    {base64:decode(ATD), base64:decode(RTD)}.

get_auth_method()        ->
    XMPPDomain = escalus_ejabberd:unify_str_arg(
                   ct:get_config(ejabberd_domain)),
    escalus_ejabberd:rpc(ejabberd_auth, store_type,
                         [XMPPDomain]).

set_store_password(Type) ->
    XMPPDomain = escalus_ejabberd:unify_str_arg(
                   ct:get_config(ejabberd_domain)),
    AuthOpts = escalus_ejabberd:rpc(ejabberd_config, get_local_option,
                                    [{auth_opts, XMPPDomain}]),
    NewAuthOpts = lists:keystore(password_format, 1, AuthOpts, {password_format, Type}),
    escalus_ejabberd:rpc(ejabberd_config, add_local_option,
                         [{auth_opts, XMPPDomain}, NewAuthOpts]).

config_password_format(login_scram) ->
    set_store_password(scram);
config_password_format(_) ->
    set_store_password(plain).

assert_password_format(GroupName, Config) ->
    Users = proplists:get_value(escalus_users, Config),
    [verify_format(GroupName, User) || User <- Users],
    Config.

verify_format(GroupName, {_User, Props}) ->
    Username = escalus_utils:jid_to_lower(proplists:get_value(username, Props)),
    Server = proplists:get_value(server, Props),
    Password = proplists:get_value(password, Props),
    SPassword = escalus_ejabberd:rpc(ejabberd_auth, get_password, [Username, Server]),
    do_verify_format(GroupName, Password, SPassword).

do_verify_format(login_scram, _Password, SPassword) ->
    %% returned password is a tuple containing scram data
    {_, _, _, _} = SPassword;
do_verify_format(_, Password, SPassword) ->
    Password = SPassword.

%% @doc Set Fields of the Record to Values,
%% when {Field, Value} <- FieldValues (in list comprehension syntax).
record_set(Record, FieldValues) ->
    F = fun({Field, Value}, Rec) ->
                setelement(Field, Rec, Value)
        end,
    lists:foldl(F, Record, FieldValues).

mimctl(Config, CmdAndArgs) ->
    Node = ct:get_config(ejabberd_node),
    ejabberd_node_utils:call_ctl_with_args(Node, convert_args(CmdAndArgs), Config).

convert_args(Args) -> [ convert_arg(A) || A <- Args ].

convert_arg(B) when is_binary(B) -> binary_to_list(B);
convert_arg(A) when is_atom(A) -> atom_to_list(A);
convert_arg(S) when is_list(S) -> S.

clean_token_db() ->
    Q = [<<"DELETE FROM auth_token">>],
    ODBCHost = <<"localhost">>, %% mam is also tested against local odbc
    R = escalus_ejabberd:rpc(ejabberd_odbc, sql_query, [ODBCHost, Q]),
    ct:pal(" ~n clean auth_token result ~n~p", [R]).
