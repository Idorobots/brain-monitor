-module(monitor_app).
-behaviour(application).

-export([start/2, stop/1]).

%% Application callbacks
start(_StartType, _StartArgs) ->
    Timeout = monitor:get_env(interval),
    monitor_sup:start_link(Timeout).

stop(_State) ->
    ok.
