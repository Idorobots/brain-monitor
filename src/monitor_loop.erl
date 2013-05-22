-module(monitor_loop).

-export([start_link/1, loop/1]).

start_link(Timeout) ->
    init(Timeout),
    Pid = spawn_link(?MODULE, loop, [Timeout]),
    {ok, Pid}.

init(_Timeout) ->
    erlang:system_flag(scheduler_wall_time, true),
    lists:map(fun (Metric) ->
                      folsom_metrics:new_histogram(Metric, slide, monitor:get_env(window))
              end,
              [cpu_utilization | monitor:get_env(metrics)]),
    ok.

%% Internal functions
loop(State) ->
    Timeout = State,
    receive
    after
        Timeout -> case handle(State) of
                       {ok, State}   -> loop(State);
                       {stop, State} -> State
                   end
    end.

handle(Timeout) ->
    Metrics = monitor:get_env(metrics),

    %% TODO Deuglify
    VMStatFuncs = [fun folsom_vm_metrics:get_memory/0,
                   fun folsom_vm_metrics:get_statistics/0],
    lists:map(fun (Fun) ->
                      Values = lists:filter(fun ({Metric, _Value}) ->
                                                    lists:member(Metric, Metrics)
                                            end,
                                            Fun()),
                      lists:map(fun folsom_metrics:notify/1, Values)
              end,
              VMStatFuncs),
    %% TODO Compute the actual utilization.
    SWT = erlang:statistics(scheduler_wall_time),
    folsom_metrics:notify({cpu_utilization, SWT}),
    {ok, Timeout}.
