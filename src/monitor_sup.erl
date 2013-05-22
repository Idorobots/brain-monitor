
-module(monitor_sup).
-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

%% API functions
start_link(Timeout) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Timeout).

%% Supervisor callbacks
init(Timeout) ->
    Strategy = {one_for_one, 5, 10},
    Processes = [child_spec(monitor_loop, Timeout)],
    {ok, {Strategy, Processes}}.

child_spec(M, Timeout) ->
    {M,
     {M, start_link, [Timeout]},
     permanent,
     5000,
     worker,
     [M]}.
