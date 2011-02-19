-module(epkgblender_app).
-behavior(application).
-export([start/0]).
-export([start/2, stop/1]).

%%
%% API
%%
start() ->
    application:start(mnesia),
    application:start(nprocreg),
    application:start(epkgblender).

%%
%% Callbacks
%%
start(_StartType, _StartArgs) ->
    epkgblender_sup:start_link().

stop(_State) ->
    ok.
