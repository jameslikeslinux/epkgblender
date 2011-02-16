-module(epkgbuilder_app).

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(epkgbuilder).

start(_StartType, _StartArgs) ->
    epkgbuilder_sup:start_link().

stop(_State) ->
    ok.
