-module(epkgblender_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%
%% API
%%
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%
%% Callbacks
%%
init([]) ->
 %   application:start(nprocreg),
    {ok, {{one_for_one, 5, 10}, [
        ?CHILD(epkgblender_user_server, worker)
    ]}}.
