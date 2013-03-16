%% @doc Module for serving a (github) Webhook which updates the site
%% which the module is installed in.

%% Copyright 2013 Arjan Scherpenisse
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

-module(mod_github).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-behaviour(gen_server).

-mod_title("Github webhook").
-mod_description("Provides a webhook to add to Github to update the site.").
-mod_prio(50).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
         make/1,
         event/2,
         pid_observe_github_make/3
]).

-include_lib("zotonic.hrl").

-record(state, {context}).

%%====================================================================
%% API
%%====================================================================

%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

make(Context) ->
    z_notifier:notify(github_make, Context).

pid_observe_github_make(Pid, github_make, _Context) ->
     gen_server:cast(Pid, make).

event(#postback{message=webhook}, Context) ->
    make(Context),
    z_render:growl(?__("A git pull is done in the background. Check the admin log for the details.", Context), Context).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    {ok, #state{
        context  = z_context:new(Context)
    }}.


handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


handle_cast(make, State) ->
    do_make(State#state.context),
    z_utils:flush_message(make),
    {noreply, State};

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

do_make(Context) ->
    Dir = z_path:site_dir(Context),
    Out = os:cmd("cd " ++ z_utils:os_escape(Dir) ++ " && git pull && echo $?"),
    ?zInfo(Out, Context),
    %% make all
    z:m(),
    %% flush site
    z_depcache:flush(Context),
    %% reload translations
    z_trans_server:load_translations(Context),
    ?zInfo(?__("Webhook-triggered recompile complete.", Context), Context),
    ok.

