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
     {commands, [], [revoke_token_cmd_when_no_token,
                     revoke_token_cmd]},
     {cleanup, [], [token_removed_on_user_removal]}
    ].

token_login_tests() ->
    [
     request_tokens_test,
     login_access_token_test,
     login_refresh_token_test,
     login_with_other_users_token
    ].

token_revocation_tests() ->
    [
     login_with_revoked_token_test,
     token_revocation_test
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------
init_per_suite(Config0) ->
    Config = dynamic_modules:stop_running(mod_last, Config0),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    dynamic_modules:start_running(Config),
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
            assert_password_format(GroupName, Config2)
    end.

end_per_group(cleanup, Config) ->
    escalus:delete_users(Config, {by_name, [alice]});
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
    RevokedToken0 = record_set(Token, [{5, invalid_sequence_no(ValidSeqNo)},
                                       {6, undefined},
                                       {7, undefined}]),
    RevokedToken = escalus_ejabberd:rpc(mod_auth_token, token_with_mac, [RevokedToken0]),
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
                                               escalus:send(Client, IQ),
                                               Result = escalus:wait_for_stanza(Client),
                                               {AT, RT} = extract_tokens(Result),
                                               Self ! {tokens, Ref, {AT,RT}}
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

%% Scenario describing JID spoofing with an eavesdropped / stolen token.
login_with_other_users_token(Config) ->
    %% given user and another user's token
    {_, JohnsToken} = request_tokens_once_logged_in_impl(Config, john),
    AliceSpec = user_authenticating_with_token(Config, alice, JohnsToken),
    %% when we try to log in
    ConnSteps = [start_stream,
                 stream_features,
                 maybe_use_ssl,
                 authenticate,
                 fun (Alice, Props, Features) ->
                         escalus:send(Alice, escalus_stanza:bind(<<"test-resource">>)),
                         BindReply = escalus_connection:get_stanza(Alice, bind_reply),
                         {Alice, [{bind_reply, BindReply} | Props], Features}
                 end],
    {ok, _, Props, _} = escalus_connection:start(AliceSpec, ConnSteps),
    %% then the server recognizes us as the other user
    LoggedInAs = extract_bound_jid(proplists:get_value(bind_reply, Props)),
    true = escalus_utils:get_username(LoggedInAs) /= escalus_users:get_username(Config, AliceSpec).

login_refresh_token_impl(Config, {_AccessToken, RefreshToken}) ->
    JohnSpec = escalus_users:get_userspec(Config, john),

    ConnSteps = [start_stream,
                 stream_features,
                 maybe_use_ssl,
                 maybe_use_compression
                ],

    {ok, ClientConnection, Props, _Features} = escalus_connection:start(JohnSpec, ConnSteps),
    Props2 = lists:keystore(oauth_token, 1, Props, {oauth_token, RefreshToken}),
    AuthResultToken = (catch escalus_auth:auth_sasl_oauth(ClientConnection, Props2)),
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
    Props2 = lists:keystore(oauth_token, 1, Props, {oauth_token, Token}),
    AuthResult = (catch escalus_auth:auth_sasl_oauth(ClientConnection, Props2)),
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
    escalus_ejabberd:rpc(mod_auth_token, revoke, [Owner]).

revoke_token_cmd_when_no_token(Config) ->
    %% given existing user with no token
    %% when revoking token
    R = mimctl(Config, ["revoke_token", escalus_users:get_jid(Config, john)]),
    %% then no token was found
    "User or token not found.\n" = R.

revoke_token_cmd(Config) ->
    %% given existing user and token present in the database
    _Tokens = request_tokens_once_logged_in_impl(Config, john),
    %% when
    R = mimctl(Config, ["revoke_token", escalus_users:get_jid(Config, john)]),
    %% then
    "Revoked.\n" = R.

token_removed_on_user_removal(Config) ->
    %% given existing user with token and XMPP (de)registration available
    _Tokens = request_tokens_once_logged_in_impl(Config, john),
    true = is_xmpp_registration_available(escalus_users:get_server(Config, john)),
    %% when user account is deleted
    S = fun (John) ->
                IQ = escalus_stanza:remove_account(),
                escalus:send(John, IQ),
                timer:sleep(500),
                escalus:assert(is_iq_result, [IQ], escalus:wait_for_stanza(John))
        end,
    escalus:story(Config, [{john, 1}], S),
    %% then token database doesn't contain user's tokens
    {selected, _, []} = get_users_token(Config, john).

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
    {updated, _} = escalus_ejabberd:rpc(ejabberd_odbc, sql_query, [ODBCHost, Q]).

get_users_token(C, User) ->
    Q = ["SELECT * FROM auth_token at "
         "WHERE at.owner = '", escalus_users:get_jid(C, User), "';"],
    escalus_ejabberd:rpc(ejabberd_odbc, sql_query,
                         [escalus_users:get_server(C, User), Q]).

is_xmpp_registration_available(Domain) ->
    escalus_ejabberd:rpc(gen_mod, is_loaded, [Domain, mod_register]).

user_authenticating_with_token(Config, UserName, Token) ->
    Spec1 = lists:keystore(oauth_token, 1, escalus_users:get_userspec(Config, UserName),
                           {oauth_token, Token}),
    lists:keystore(auth, 1, Spec1, {auth, {escalus_auth, auth_sasl_oauth}}).

extract_bound_jid(BindReply) ->
    exml_query:path(BindReply, [{element, <<"bind">>}, {element, <<"jid">>},
                                cdata]).
