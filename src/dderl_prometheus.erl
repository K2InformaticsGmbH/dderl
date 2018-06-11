-module(dderl_prometheus).

-include("dderl.hrl").

-export([init/2]).

-define(CONFIG,
        #{process_count => #{type => gauge, help => "Process Count"},
          port_count => #{type => gauge, help => "Number of Ports used"},
          run_queue => #{type => gauge, help => "Run Queue Size"},
          memory => #{type => gauge, help => "Memory"},
          data_nodes => #{type => gauge, help => "Number of nodes in the cluster"}
         }).

-define(VALUESFUN,
        <<"fun(Config) ->"
          "  #{data_nodes := DNodes} = imem_metrics:get_metric(data_nodes),"
          "  #{port_count := Ports,process_count := Procs,erlang_memory := Memory,"
          "    run_queue := RunQ} = imem_metrics:get_metric(system_information),"
          "  maps:fold("
          "      fun(memory, _V, Acc) -> Acc#{memory => Memory};"
          "         (process_count, _V, Acc) -> Acc#{process_count => Procs};"
          "         (port_count, _V, Acc) -> Acc#{port_count => Ports};"
          "         (run_queue, _V, Acc) -> Acc#{run_queue => RunQ};"
          "         (data_nodes, _V, Acc) -> Acc#{data_nodes => length(DNodes)};"
          "         (K, V, Acc) -> Acc"
          "      end, #{}, Config)"
          "end.">>
    ).

-define(METRICS, ?GET_CONFIG(prometheusMetrics, [], ?CONFIG,
                             "Prometheus Metrics")).
-define(METRICS_FUN, ?GET_CONFIG(prometheusMetricsFun, [], ?VALUESFUN,
                                 "Prometheus Metrics values function, which "
                                 "returns a map with all the metrics keys and "
                                 "their corresponding values")).
-define(AUTHORIZATION, ?GET_CONFIG(prometheusAuth, [],
                                   #{user => "admin", password => "test"},
                                   "Prometheus Scraper Basic credentials")).

init(#{headers := #{<<"authorization">> := Auth},
       method := <<"GET">>} = Req, metrics) ->
    Req1 =
    case check_auth(Auth) of
        true ->
            Metrics = get_metrics(),
            case get_metrics_fun() of
                {error, _Error} -> cowboy_req:reply(500, #{}, <<>>, Req);
                {ok, Fun} -> execute_metrics_fun(Fun, Metrics, Req)
            end;
        false -> cowboy_req:reply(401, #{}, <<>>, Req)
    end,
    {ok, Req1, undefined};
init(Req, _) ->
    Req1 = cowboy_req:reply(404, #{}, <<>>, Req),
    {ok, Req1, undefined}.

get_metric(Name, #{type := Type, help := Help} = Spec) -> 
    case metric_type(Type) of
        not_valid -> {error, not_valid};
        MetricType ->
            OtherInfos = get_other_infos(Spec),
            {MetricType, [{name, Name}, {help, Help} | OtherInfos]}
    end;
get_metric(_, _) -> {error, not_valid}.

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

set_metric(MetricType, SetterFun, Name, {Labels, Value}) ->
    MetricType:SetterFun(Name, Labels, Value);
set_metric(MetricType, SetterFun, Name, Value) ->
    MetricType:SetterFun(Name, Value).

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

set_fun(Type) when Type == gauge; Type == boolean -> set;
set_fun(Type) when Type == summary; Type == histogram -> observe;
set_fun(counter) -> inc.

get_metrics() ->
    PMetrics = ?METRICS,
    CacheMetricsKey = {?MODULE, prometheusMetrics},
    case imem_cache:read(CacheMetricsKey) of 
        [PMetrics] -> PMetrics;
        _ ->
            imem_cache:write(CacheMetricsKey, PMetrics),
            declare_metrics(PMetrics),
            PMetrics
    end.

get_metrics_fun() ->
    FunStr = ?METRICS_FUN,
    CacheFunKey = {?MODULE, prometheusFun, FunStr},
    case imem_cache:read(CacheFunKey) of
        [] ->
            case catch imem_compiler:compile(FunStr) of
                NewFun when is_function(NewFun) -> 
                    imem_cache:write(CacheFunKey, NewFun),
                    {ok, NewFun};
                Error ->
                    ?Error("compileing fun : ~p", [Error]),
                    {error, Error}
            end;
        [OldFun] -> {ok, OldFun}
    end.

declare_metrics(Metrics) ->
    prometheus_registry:clear(),
    maps:map(fun(Name, Info) ->
        case get_metric(Name, Info) of
            {error, not_valid} ->
                ?Error("~p : ~p - not supported", [Name, Info]);
            {MetricType, Spec} ->
                MetricType:declare(Spec)
        end
    end, Metrics).

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

check_auth(<<"Basic ", Auth64/binary>>) ->
    #{user := User, password := Password} = ?AUTHORIZATION,
    list_to_binary([User, ":", Password]) == base64:decode(Auth64);
check_auth(_) -> false.