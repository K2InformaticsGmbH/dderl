-module(dderl_prometheus).

-include("dderl.hrl").

-export([init/0]).
-export([init/2]).

-define(CONFIG, [#{name => memory, type => guage, 
                   labels => ["memory", "system"], help => "memory"}]).

-define(METRICS, ?GET_CONFIG(prometheusMetrics, [], ?CONFIG, "Prometheus Metrics")).

init() ->
    init(?METRICS).

init([]) -> ok;
init([Config | Configs]) ->
    {MetricType, Spec} = get_metric(Config),
    MetricType:declare(Spec),
    init(Configs).

init(#{method := <<"GET">>} = Req, metrics) ->
    [set_metric(Metric) || Metric <- ?METRICS],
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, 
                            prometheus_text_format:format(), Req),
    {ok, Req1, undefined};
init(Req, _) ->
    {stop, Req}.

get_metric(#{type := guage, name := Name, help := Help}) -> 
    {prometheus_gauge, [{name, Name}, {help, Help}]};
get_metric(#{type := summary, name := Name, help := Help}) -> 
    {prometheus_summary, [{name, Name}, {help, Help}]};
get_metric(#{type := counter, name := Name, help := Help}) -> 
    {prometheus_counter, [{name, Name}, {help, Help}]};
get_metric(#{type := histogram, name := Name, help := Help}) ->
    {prometheus_histogram, [{name, Name}, {help, Help}]}.

set_metric(#{type := guage, name := Name}) -> 
    prometheus_gauge:set(Name, get_value(Name));
set_metric(#{type := summary, name := Name}) -> 
    prometheus_summary:set(Name, get_value(Name));
set_metric(#{type := counter, name := Name}) -> 
    prometheus_counter:set(Name, get_value(Name));
set_metric(#{type := histogram, name := Name}) ->
    prometheus_histogram:set(Name, get_value(Name)).

get_value(memory) -> erlang:memory(total).