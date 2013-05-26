-module(monitor_loop).

-export([start_link/1, loop/1]).

-record(state, {timeout, swt, metrics}).

start_link(Timeout) ->
    Pid = spawn_link(?MODULE, loop, [init(Timeout)]),
    {ok, Pid}.

init(Timeout) ->
    erlang:system_flag(scheduler_wall_time, true),
    SWT = lists:sort(erlang:statistics(scheduler_wall_time)),
    lists:map(fun (Metric) ->
                      folsom_metrics:new_histogram(Metric, slide, monitor:get_env(window))
              end,
              [cpu_utilization | monitor:get_env(metrics)]),
    Metrics = monitor:get_env(metrics), % Pass metrics along the loop not to invoke get_env too much.
    #state{timeout = Timeout, swt = SWT, metrics = Metrics}.

%% Internal functions
loop(State) ->
    Timeout = State#state.timeout,
    receive
    after
        Timeout -> case handle(State) of
                       {ok, NewState}   -> loop(NewState);
                       {stop, NewState} -> NewState
                   end
    end.

handle(State) ->
    Metrics = State#state.metrics,

    VMStatFuncs = [fun folsom_vm_metrics:get_memory/0,
                   fun folsom_vm_metrics:get_statistics/0],
    lists:map(fun (Fun) ->
                      lists:map(fun folsom_metrics:notify/1,
                                lists:filter(fun ({Metric, _Value}) ->
                                                     lists:member(Metric, Metrics)
                                             end,
                                             Fun()))
              end,
              VMStatFuncs),

    %% Lastly, compute and update the CPU utilization.
    LastSWT = State#state.swt,
    NewSWT = lists:sort(erlang:statistics(scheduler_wall_time)),
    CPUUtilization = lists:map(fun({{I, A0, T0}, {I, A1, T1}}) ->
                                       {I, (A1-A0)/(T1-T0)}
                               end,
                               lists:zip(LastSWT, NewSWT)),
    folsom_metrics:notify({cpu_utilization, CPUUtilization}),
    {ok, State#state{swt = NewSWT}}.
