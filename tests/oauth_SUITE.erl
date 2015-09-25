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
     login_access_token_test
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
    % mongoose_helper:clear_last_activity(Config, AnonJID),
    escalus:end_per_testcase(CaseName, Config).


%% alice makes request and should get access and refresh tokens from the server.
request_tokens_test(Config) ->
    request_tokens_impl(Config).


request_tokens_impl(Config) ->
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
                                               ct:pal("~n extracted tokens: ~p ~n ~p ~n " , [AT,RT])
                                       end),
    receive
        {tokens, Ref, Tokens} ->
            Tokens
    after
        1000 -> error
    end.

login_access_token_test(Config) ->
    Tokens = request_tokens_impl(Config),
    login_access_token_impl(Config, Tokens).

%% users logs in using access token he obtained in previous session (stream has been
%% already reset)
login_access_token_impl(Config, {AccessToken, _RefreshToken}) ->
    ct:pal( "login_access_token_impl config: ~n~p~n:", [Config]),

    Users = proplists:get_value(escalus_users, Config),
    ct:pal( " ~n ------ users from config ~p ~n ", [Users]),
    JohnSpec = escalus_users:get_userspec(Config, john),
    ct:pal( " ~n ------ john spec   ~p ~n ", [JohnSpec]),

    ConnSteps = [start_stream,
                        stream_features,
                        maybe_use_ssl,
                        maybe_use_compression
                        ],

    {ok, Connection, Props, Features} = escalus_connection:start(JohnSpec, ConnSteps),

    ct:pal( " ~n ------Stream Features [0]   ~p ~n ", [Features]),

    Props2 = lists:keystore(access_token, 1, Props, {access_token, AccessToken}),

    AuthResult = (catch escalus_auth:auth_sasl_oauth(Connection, Props2)),
    ct:pal( " ~n ------ SASL auth result   ~p ~n ", [AuthResult]),

    escalus_connection:reset_parser(Connection),
    {Props3, []} = escalus_session:start_stream(Connection, Props2),
    NewFeatures = escalus_session:stream_features(Connection, Props3, []),
    ct:pal( " ~n ------ Stream Features [1]  ~p ~n ", [NewFeatures]).


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


