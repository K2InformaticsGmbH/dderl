-module(dderl_prometheus).

-behavior(cowboy_rest).

-include("dderl.hrl").

% cowboy_handler callback
-export([init/2, is_authorized/2, allowed_methods/2, content_types_provided/2,
         get_metrics/2]).

-define(DEFAULT_METRICS,
        #{process_count => #{type => gauge, help => "Process Count", labels => ["node","schema"]},
          port_count => #{type => gauge, help => "Number of Ports used", labels => ["node","schema"]},
          run_queue => #{type => gauge, help => "Run Queue Size", labels => ["node","schema"]},
          memory => #{type => gauge, help => "Memory", labels => ["node","schema"]},
          data_nodes => #{type => gauge, help => "Number of nodes in the cluster", labels => ["node","schema"]}
         }).

-define(DEFAULT_METRICS_FUN,
        <<"fun(Config) ->"
          "  #{data_nodes := DNodes} = imem_metrics:get_metric(data_nodes),"
          "  #{port_count := Ports,process_count := Procs,erlang_memory := Memory,"
          "    run_queue := RunQ} = imem_metrics:get_metric(system_information),"
          "  Labels = [node(), imem_meta:schema()],"
          "  maps:fold("
          "      fun(memory, _V, Acc) -> Acc#{memory => {Labels, Memory}};"
          "         (process_count, _V, Acc) -> Acc#{process_count => {Labels, Procs}};"
          "         (port_count, _V, Acc) -> Acc#{port_count => {Labels, Ports}};"
          "         (run_queue, _V, Acc) -> Acc#{run_queue => {Labels, RunQ}};"
          "         (data_nodes, _V, Acc) -> Acc#{data_nodes => {Labels, length(DNodes)}};"
          "         (K, V, Acc) -> Acc"
          "      end, #{}, Config)"
          "end.">>
    ).

-define(ISENABLED, ?GET_CONFIG(prometheusIsEnabled, [], false,
                             "Prometheus Metrics Enable Flag")).
-define(METRICS, ?GET_CONFIG(prometheusMetrics, [], ?DEFAULT_METRICS,
                             "Prometheus Metrics")).
-define(METRICS_FUN, ?GET_CONFIG(prometheusMetricsFun, [], ?DEFAULT_METRICS_FUN,
                                 "Prometheus Metrics values function, which "
                                 "returns a map with all the metrics keys and "
                                 "their corresponding values")).
-define(AUTHORIZATION, ?GET_CONFIG(prometheusAuth, [],
                                   #{user => "admin", password => "test"},
                                   "Prometheus Scraper Basic credentials")).

init(Req, metrics) ->
    {cowboy_rest, Req, undefined}.

is_authorized(#{headers := #{<<"authorization">> := <<"Basic ", Auth64/binary>>}} = Req, State) ->
    #{user := User, password := Password} = ?AUTHORIZATION,
    Auth = base64:decode(Auth64),
    Result =
    case list_to_binary([User, ":", Password]) of
        Auth -> true;
        _ -> {false, <<"Basic">>}
    end,
    {Result, Req, State};
is_authorized(Req, State) ->
    {{false, <<"Basic">>}, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"plain/text">>, get_metrics}], Req, State}.

get_metrics(Req, State) ->
    Req1 =
    case metrics_fun() of
        {error, _Error} -> cowboy_req:reply(500, #{}, <<>>, Req);
        {ok, Fun} -> execute_metrics_fun(Fun, metrics(), Req)
    end,
    {stop, Req1, State}.

%private
metrics_fun() ->
    FunStr = ?METRICS_FUN,
    CacheFunKey = {?MODULE, prometheusFun},
    case imem_cache:read(CacheFunKey) of
        [{FunStr, OldFun}] -> {ok, OldFun};
        _ ->
            case catch imem_compiler:compile(FunStr) of
                Fun when is_function(Fun, 1) ->
                    imem_cache:write(CacheFunKey, {FunStr, Fun}),
                    {ok, Fun};
                Error ->
                    ?Error("compiling fun : ~p", [Error]),
                    {error, Error}
            end
    end.

metrics() ->
    PMetrics = ?METRICS,
    CacheMetricsKey = {?MODULE, prometheusMetrics},
    case imem_cache:read(CacheMetricsKey) of 
        [PMetrics] -> PMetrics;
        _ ->
            imem_cache:write(CacheMetricsKey, PMetrics),
            declare_metrics(PMetrics),
            PMetrics
    end.

declare_metrics(Metrics) ->
    prometheus_registry:clear(),
    maps:map(fun(Name, Info) ->
        case metric(Name, Info) of
            {error, not_valid} ->
                ?Error("~p : ~p - not supported", [Name, Info]);
            {MetricType, Spec} ->
                MetricType:declare(Spec)
        end
    end, Metrics).

metric(Name, #{type := Type, help := Help} = Spec) -> 
    case metric_type(Type) of
        not_valid -> {error, not_valid};
        MetricType ->
            OtherInfos = get_other_infos(Spec),
            {MetricType, [{name, Name}, {help, Help} | OtherInfos]}
    end;
metric(_, _) -> {error, not_valid}.

metric_type(gauge) -> prometheus_gauge;
metric_type(boolean) -> prometheus_boolean;
metric_type(summary) -> prometheus_summary;
metric_type(counter) -> prometheus_counter;
metric_type(histogram) -> prometheus_histogram;
metric_type(_) -> not_valid.

get_other_infos(Spec) ->
    maps:fold(
        fun(labels, Labels, Acc) -> [{labels, Labels} | Acc];
           (buckets, Buckets, Acc) -> [{buckets, Buckets} | Acc];
           (duration_unit, DUnit, Acc) -> [{duration_unit, DUnit} | Acc];
           (_, _, Acc) -> Acc
        end, [], Spec).

execute_metrics_fun(Fun, Metrics, Req) ->
    case catch Fun(Metrics) of
        Results when is_map(Results) ->
            maps:map(fun(Name, Spec) ->
                case maps:get(Name, Results, none) of
                    none -> ?Error("Required metric ~p not found", [Name]);
                    Value -> set_metric_value(Name, Value, Spec)
                end
            end, Metrics),
            cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>},
                            prometheus_text_format:format(), Req);
        Error ->
            ?Error("Invalid result from prometheus fun : ~p", [Error]),
            cowboy_req:reply(500, #{}, <<>>, Req)
    end.

set_metric_value(Name, Value, Spec) ->
    try set_metric(Name, Value, Spec)
    catch
        C:E ->
            ?Error("Setting ~p with ~p for spec ~p resulted in ~p",
                   [Name, Value, Spec, {C, E}])
    end.

set_metric(Name, Value, #{type := Type}) ->
    MetricType = metric_type(Type),
    SetterFun = set_fun(Type),
    set_metric(MetricType, SetterFun, Name, Value).

set_fun(Type) when Type == gauge; Type == boolean -> set;
set_fun(Type) when Type == summary; Type == histogram -> observe;
set_fun(counter) -> inc.

set_metric(MetricType, SetterFun, Name, {Labels, Value}) ->
    MetricType:SetterFun(Name, Labels, Value);
set_metric(MetricType, SetterFun, Name, Value) ->
    MetricType:SetterFun(Name, Value).