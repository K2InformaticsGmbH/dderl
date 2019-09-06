-module(dderl_metrics).

-include("dderl.hrl").

-behaviour(imem_gen_metrics).

-export([start_link/0
        ,get_metric/1
        ,get_metric/2
        ,request_metric/3
        ]).

-export([init/0
        ,handle_metric_req/3
        ,request_metric/1
        ,terminate/2
        ]).

-safe(get_metric).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    imem_gen_metrics:start_link(?MODULE).

-spec get_metric(term()) -> term().
get_metric(MetricKey) ->
    imem_gen_metrics:get_metric(?MODULE, MetricKey).

-spec get_metric(term(), integer()) -> term().
get_metric(MetricKey, Timeout) ->
    imem_gen_metrics:get_metric(?MODULE, MetricKey, Timeout).

-spec request_metric(term(), term(), pid()) -> term().
request_metric(MetricKey, ReqRef, ReplyTo) ->
    imem_gen_metrics:request_metric(?MODULE, MetricKey, ReqRef, ReplyTo).

%% imem_gen_metrics callback
init() -> {ok, undefined}.

handle_metric_req(session_count, ReplyFun, State) ->
    SessionCount =
    case whereis(dderl_session_sup) of
        Pid when is_pid(Pid) ->
            case erlang:process_info(Pid, links) of
                {links, Links} ->
                    % subtracting one to remove pid of dderl_sup
                    length(Links) - 1;
                _ ->
                    0
            end;
        _ ->
            0
    end,
    ReplyFun(SessionCount),
    State;
handle_metric_req(UnknownMetric, ReplyFun, State) ->
    ?Error("Unknow metric requested ~p when state ~p", [UnknownMetric, State]),
    ReplyFun({error, unknown_metric}),
    State.

request_metric(_) -> noreply.

terminate(_Reason, _State) -> ok.
