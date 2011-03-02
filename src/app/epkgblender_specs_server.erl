%%%
%%% epkgblender_specs_server.erl
%%% Copyright (C) 2011 James Lee
%%% 
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%

-module(epkgblender_specs_server).
-author("James Lee <jlee@thestaticvoid.com>").
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("stdlib/include/qlc.hrl").

% callbacks may take some time since they are doing git commands
-define(TIMEOUT, 30000).


%%
%% API
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{timeout, ?TIMEOUT}]).


%%
%% Callbacks
%%
init([]) ->
    case {application:get_env(specs_repo), application:get_env(specs_dir)} of
        {{ok, SpecsRepo}, {ok, SpecsDir}} ->
            % initialize repository directory
            % git won't overwrite an existing repository, so this is safe to run over and over
            epkgblender_utils:run_cmd("git clone " ++ SpecsRepo ++ " " ++ SpecsDir),

            % set repository username and email
            % the email address is registered with GitHub
            file:set_cwd(SpecsDir),
            epkgblender_utils:run_cmd("git config user.name pkgblender"),
            epkgblender_utils:run_cmd("git config user.email epkgblender@pkgblender.org"),

            % update repo
            case epkgblender_utils:run_cmd("git pull") of
                {0, _Output} ->
                    {ok, []};
                _ ->
                    {stop, git_pull_failed}
            end;
        _ ->
            {stop, specs_env_undefined}
    end.

handle_call(_Msg, _From, State) -> {noreply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%
%% Private Functions
%%
