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
    [{group, oauth}].

groups() ->
    [{oauth, [sequence], all_tests()}].

all_tests() ->
    [request_tokens_test,
     login_access_token_test,
     login_refresh_token_test,
     login_with_revoked_token_test
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

init_per_group(GroupName, Config) ->
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
    escalus:init_per_testcase(CaseName, Config0).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

request_tokens_test(Config) ->
    request_tokens_once_logged_in_impl(Config).

login_with_revoked_token_test(Config) ->
    %% given
    S = fun(John) ->
                Token = get_revoked_token(John),
                self() ! {revoked_token, Token}
        end,
    escalus:story(Config, [{john, 1}], S),
    RevokedToken = receive
                       {revoked_token, T} -> T
                   end,
    {{auth_failed, _}, _, _} = login_with_token(Config, john, RevokedToken).

get_revoked_token(Client) ->
    XMPPDomain = escalus_client:server(Client),
    BJID = escalus_client:short_jid(Client),
    JID = escalus_ejabberd:rpc(jlib, binary_to_jid, [BJID]),
    Token = escalus_ejabberd:rpc(mod_auth_token, token, [refresh, JID]),
    ValidSeqNo = escalus_ejabberd:rpc(mod_auth_token_backend, get_sequence_number,
                                      [refresh, JID]),
    ct:pal( "~n ValidSEQ ~n~p", [ValidSeqNo]),
    RevokedToken = setelement(5, Token, invalid_sequence_no(ValidSeqNo)),
    escalus_ejabberd:rpc(mod_auth_token, serialize, [RevokedToken]).

invalid_sequence_no(SeqNo) ->
    SeqNo + 1.

request_tokens_once_logged_in_impl(Config) ->
    Self = self(),
    Ref = make_ref(),
    escalus:story(Config, [{john, 1}], fun(John) ->
                                               JohnShortJid = escalus_utils:get_short_jid(John),
                                               R = escalus_stanza:query_el(?NS_AUTH_TOKEN, []),
                                               IQ = escalus_stanza:iq(JohnShortJid, <<"get">>, R),
                                                % ct:pal("--test IQ w query ~p " , [IQ]),
                                               escalus:send(John, IQ),
                                               Result = escalus:wait_for_stanza(John),
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
    Tokens = request_tokens_once_logged_in_impl(Config),
    login_access_token_impl(Config, Tokens).


 login_refresh_token_test(Config) ->
    Tokens = request_tokens_once_logged_in_impl(Config),
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
    {NewClientConnection2, Props5, NewFeatures3} =
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
    ct:pal( " ~n ------ SASL (access token) auth result   ~p ~n ", [AuthResult]),
    {AuthResult, ClientConnection, Props2}.

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


