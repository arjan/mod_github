%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%% @doc Redirect to the authorize uri of GitHub
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

-module(controller_github_authorize).
-author("Marc Worrell <marc@worrell.nl>").

-export([init/1, service_available/2, charsets_provided/2, content_types_provided/2]).
-export([resource_exists/2, previously_existed/2, moved_temporarily/2]).
-export([to_html/2]).
-export([get_args/1]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("include/zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    z_context:lager_md(Context),
    Context1 = z_context:set(DispatchArgs, Context),
    Context2 = z_context:ensure_all(Context1),
    ?WM_REPLY(true, Context2).

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", provide_content}], ReqData, Context}.

resource_exists(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    case z_context:get_q("error", Context1) of
        undefined -> ?WM_REPLY(false, Context1);
        _ -> ?WM_REPLY(true, Context1)
    end.

previously_existed(ReqData, Context) ->
    {true, ReqData, Context}.

moved_temporarily(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Location = redirect_location(Context1),
    save_args(Context1),
    ?WM_REPLY({true, Location}, Context1).

to_html(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1),
    z_context:lager_md(Context2),
    {Result, ResultContext} = html(Context2),
    ?WM_REPLY(Result, ResultContext).

html(Context) ->
    Error = z_context:get_q("error_description", Context),
    Vars = [
        {service, "GitHub"},
        {error, Error}
    ],
    Html = z_template:render("logon_service_error.tpl", Vars, Context),
    z_context:output(Html, Context).

redirect_location(Context) ->
    GitHubState = z_ids:id(),
    z_context:set_session(github_state, GitHubState, Context),
    {AppId, _AppSecret, Scope} = mod_github:get_config(Context),
    ContextNoLang = z_context:set_language(undefined, Context),
    RedirectUrl = z_convert:to_list(
                        z_context:abs_url(
                            z_dispatcher:url_for(github_redirect, ContextNoLang),
                            Context)),
    "https://github.com/login/oauth/authorize?"
        ++ "client_id=" ++ z_utils:url_encode(AppId)
        ++ "&redirect_uri=" ++ z_utils:url_encode(RedirectUrl)
        ++ "&state=" ++ z_utils:url_encode(GitHubState)
        ++ "&scope=" ++ z_utils:url_encode(Scope).

save_args(Context) ->
    z_context:set_session(?MODULE, z_context:get_q_all_noz(Context), Context).

get_args(Context) ->
    Args = z_context:get_session(?MODULE, Context),
    z_context:set_session(?MODULE, undefined, Context),
    case Args of
        L when is_list(L) -> L;
        undefined -> []
    end.
