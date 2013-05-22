-module(monitor).

-export([start/0, stop/0, get_env/1, stats/0, dump_stats/1]).

start() ->
    lager:start(),
    folsom:start(),
    application:start(monitor).

stop() ->
    application:stop(monitor).

%% External functions
stats() ->
    lists:map(fun (Metric) ->
                  {Metric, folsom_metrics:get_metric_value(Metric)}
              end,
              folsom_metrics:get_metrics()).

dump_stats(Filename) ->
    Stats = stats(),
    ok = file:write_file(Filename, io_lib:format("~w", [Stats])).

get_env(Name) ->
    {ok, Value} = application:get_env(?MODULE, Name),
    Value.
