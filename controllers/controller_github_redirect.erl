%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%% @doc Handle the OAuth redirect of the GitHub logon handshake.
%% See: https://developer.github.com/apps/building-integrations/setting-up-and-registering-oauth-apps/about-authorization-options-for-oauth-apps/

%% Copyright 2017 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(controller_github_redirect).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    html/1,
    fetch_user_data/1
    ]).

-include_lib("controller_html_helper.hrl").

html(Context) ->
    QState = z_context:get_q("state", Context),
    case z_context:get_session(github_state, Context) of
        undefined ->
            lager:warning("GitHub OAuth redirect with missing session state"),
            html_error(missing_secret, Context);
        QState ->
            case z_context:get_q("code", Context) of
                undefined ->
                    Context1 = z_render:wire({script, [{script, "window.close();"}]}, Context),
                    html_error(cancel, Context1);
                Code ->
                    access_token(fetch_access_token(Code, QState, Context), Context)
            end;
        SessionState ->
            lager:warning("GitHub OAuth redirect with state mismatch, expected ~p, got ~p",
                          [SessionState, QState]),
            Context1 = z_render:wire({script, [{script, "window.close();"}]}, Context),
            html_error(wrong_secret, Context1)
    end.

access_token({ok, AccessToken}, Context) ->
    Data = [
        {access_token, AccessToken}
    ],
    user_data(fetch_user_data(AccessToken), Data, Context);
access_token({error, _Reason}, Context) ->
    html_error(access_token, Context).

user_data({ok, UserProps}, AccessData, Context) ->
    case auth_user(UserProps, AccessData, Context) of
        undefined ->
            % No handler for signups, or signup not accepted
            lager:warning("[github] Undefined auth_user return for user with props ~p", [UserProps]),
            html_error(auth_user_undefined, Context);
        {error, duplicate} ->
            lager:info("[github] Duplicate connection for user with props ~p", [UserProps]),
            html_error(duplicate, Context);
        {error, _} = Err ->
            lager:warning("[github] Error return ~p for user with props ~p", [Err, UserProps]),
            html_error(auth_user_error, Context);
        {ok, Context1} ->
            html_ok(Context1)
    end;
user_data({error, _Reason}, _AccessData, Context) ->
    html_error(service_user_data, Context).


html_ok(Context) ->
    Html = z_template:render("logon_service_done.tpl", [{service, "GitHub"} | z_context:get_all(Context)], Context),
    z_context:output(Html, Context).

html_error(Error, Context) ->
    Vars = [
        {service, "GitHub"},
        {is_safari8problem, is_safari8problem(Context)},
        {error, Error}
    ],
    Html = z_template:render("logon_service_error.tpl", Vars, Context),
    z_context:output(Html, Context).

%% @doc There is a problem here with Safari 8.0.x which (in its default setting) does not pass any
%%      cookies after the redirect from LinkedIn (and other OAuth redirects).
%%      See also this issue: https://github.com/drone/drone/issues/663#issuecomment-61565820
is_safari8problem(Context) ->
    Hs = m_req:get(headers, Context),
    HasCookies = proplists:is_defined("cookie", Hs),
    UA = m_req:get(user_agent, Context),
    IsVersion8 = string:str(UA, "Version/8.0.") > 0,
    IsSafari = string:str(UA, "Safari/6") > 0,
    not HasCookies andalso IsVersion8 andalso IsSafari.


auth_user(Profile, AccessTokenData, Context) ->
    % GithubUserId = proplists:get_value(<<"id">>, Profile),
    GithubUsername = proplists:get_value(<<"login">>, Profile),
    lager:debug("[github] Authenticating ~p ~p", [GithubUsername, Profile]),
    AccessToken = proplists:get_value(access_token, AccessTokenData),
    Email = case user_email(AccessToken) of
        undefined -> get_value(<<"email">>, Profile);
        E -> E
    end,
    PersonProps = [
            {title, get_value(<<"name">>, Profile)},
            {summary, get_value(<<"bio">>, Profile)},
            {website, get_value(<<"html_url">>, Profile)},
            {github_url, get_value(<<"html_url">>, Profile)},
            {email, Email},
            {depiction_url, get_value(<<"avatar_url">>, Profile)}
        ],
    Args = controller_github_authorize:get_args(Context),
    z_notifier:first(#auth_validated{
            service=github,
            service_uid=GithubUsername,
            service_props=AccessTokenData,
            props=PersonProps,
            is_connect=z_convert:to_bool(proplists:get_value("is_connect", Args))
        },
        Context).

get_value(Prop, Data) ->
    case proplists:get_value(Prop, Data) of
        null -> undefined;
        V -> V
    end.

% Exchange the code for an access token
fetch_access_token(Code, State, Context) ->
    {AppId, AppSecret, _Scope} = mod_github:get_config(Context),
    ContextNoLang = z_context:set_language(undefined, Context),
    RedirectUrl = z_context:abs_url(z_dispatcher:url_for(github_redirect, ContextNoLang), Context),
    GitHubUrl = "https://github.com/login/oauth/access_token",
    Body = iolist_to_binary([
        "client_id=", z_utils:url_encode(AppId),
        "&client_secret=", z_utils:url_encode(AppSecret),
        "&code=", z_utils:url_encode(Code),
        "&redirect_uri=", z_convert:to_list(z_utils:url_encode(RedirectUrl)),
        "&state=", z_utils:url_encode(State)
    ]),
    case httpc:request(
            post,
            {GitHubUrl, http_headers(), "application/x-www-form-urlencoded", Body},
            httpc_http_options(),
            httpc_options())
    of
        {ok, {{_, 200, _}, _Headers, Payload}} ->
            Props = mochiweb_util:parse_qs(Payload),
            {"access_token", AccessToken} = proplists:lookup("access_token", Props),
            {ok, AccessToken};
        Other ->
            lager:error("[github] error fetching access token [code ~p] ~p", [Code, Other]),
            {error, {http_error, GitHubUrl, Other}}
    end.

% Given the access token, fetch data about the user
% See https://developer.github.com/v3/users/
fetch_user_data(AccessToken) ->
    Url = "https://api.github.com/user?access_token="++z_convert:to_list(AccessToken),
    case httpc:request(
        get,
        {Url, http_headers()},
        httpc_http_options(),
        httpc_options())
    of
        {ok, {{_, 200, _}, _Headers, Payload}} ->
            {struct, Props} = mochijson:binary_decode(Payload),
            {ok, Props};
        {ok, {{_, 401, _}, _Headers, Payload}} = Other ->
            lager:error("[github] 401 error fetching user data [token ~p] will not retry ~p", [AccessToken, Payload]),
            {error, {http_error, Url, Other}};
        Other ->
            lager:error("[github] error fetching user data [token ~p] ~p", [AccessToken, Other]),
            {error, {http_error, Url, Other}}
    end.

user_email(AccessToken) ->
    case fetch_user_emails(AccessToken) of
        {ok, Emails} ->
            Primary = lists:filter(
                fun({struct, E}) ->
                    proplists:get_value(<<"primary">>, E)
                end,
                Emails),
            Verified = lists:filter(
                fun({struct, E}) ->
                    proplists:get_value(<<"verified">>, E)
                end,
                Emails),
            case {Primary, Verified, Emails} of
                {[{struct, E}|_], _, _} -> proplists:get_value(<<"email">>, E);
                {[], [{struct, E}|_], _} -> proplists:get_value(<<"email">>, E);
                {[], [], [{struct, E}|_]} -> proplists:get_value(<<"email">>, E);
                {[], [], []} -> undefined
            end;
        {error, _} ->
            undefined
    end.

fetch_user_emails(AccessToken) ->
    Url = "https://api.github.com/user/emails?access_token="++z_convert:to_list(AccessToken),
    case httpc:request(
        get,
        {Url, http_headers()},
        httpc_http_options(),
        httpc_options())
    of
        {ok, {{_, 200, _}, _Headers, Payload}} ->
            Props = mochijson:binary_decode(Payload),
            {ok, Props};
        {ok, {{_, 401, _}, _Headers, Payload}} = Other ->
            lager:error("[github] 401 error fetching user data [token ~p] will not retry ~p", [AccessToken, Payload]),
            {error, {http_error, Url, Other}};
        Other ->
            lager:error("[github] error fetching user data [token ~p] ~p", [AccessToken, Other]),
            {error, {http_error, Url, Other}}
    end.


http_headers() ->
    [
        {"User-Agent", "Zotonic "++?ZOTONIC_VERSION}
    ].

% {
%   "login": "octocat",
%   "id": 1,
%   "avatar_url": "https://github.com/images/error/octocat_happy.gif",
%   "gravatar_id": "",
%   "url": "https://api.github.com/users/octocat",
%   "html_url": "https://github.com/octocat",
%   "followers_url": "https://api.github.com/users/octocat/followers",
%   "following_url": "https://api.github.com/users/octocat/following{/other_user}",
%   "gists_url": "https://api.github.com/users/octocat/gists{/gist_id}",
%   "starred_url": "https://api.github.com/users/octocat/starred{/owner}{/repo}",
%   "subscriptions_url": "https://api.github.com/users/octocat/subscriptions",
%   "organizations_url": "https://api.github.com/users/octocat/orgs",
%   "repos_url": "https://api.github.com/users/octocat/repos",
%   "events_url": "https://api.github.com/users/octocat/events{/privacy}",
%   "received_events_url": "https://api.github.com/users/octocat/received_events",
%   "type": "User",
%   "site_admin": false,
%   "name": "monalisa octocat",
%   "company": "GitHub",
%   "blog": "https://github.com/blog",
%   "location": "San Francisco",
%   "email": "octocat@github.com",
%   "hireable": false,
%   "bio": "There once was...",
%   "public_repos": 2,
%   "public_gists": 1,
%   "followers": 20,
%   "following": 0,
%   "created_at": "2008-01-14T04:33:35Z",
%   "updated_at": "2008-01-14T04:33:35Z",
%   "total_private_repos": 100,
%   "owned_private_repos": 100,
%   "private_gists": 81,
%   "disk_usage": 10000,
%   "collaborators": 8,
%   "two_factor_authentication": true,
%   "plan": {
%     "name": "Medium",
%     "space": 400,
%     "private_repos": 20,
%     "collaborators": 0
%   }
% }


httpc_options() ->
    [
        {sync, true},
        {body_format, binary}
    ].

httpc_http_options() ->
    [
        {timeout, 10000},
        {connect_timeout, 10000},
        {autoredirect, true},
        {relaxed, true}
    ].

