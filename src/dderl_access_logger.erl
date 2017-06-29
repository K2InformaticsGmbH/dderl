-module(dderl_access_logger).

-include("dderl.hrl").
-include_lib("lager/include/lager.hrl").        

-export([log/1, log/4]).

% library APIs
-export([src/1, proxy/1, userid/1, username/1, sessionid/1, bytes/1, time/1,
         args/1, sql/1, version/1, logLevel/1]).

% lager formatter custom interface
-export([format/2, format/3]).

-define(AccessSchema,
        [{src,       fun ?MODULE:src/1},
         {proxy,     fun ?MODULE:proxy/1},
         {userId,    fun ?MODULE:userid/1},
         {userName,  fun ?MODULE:username/1},
         {sessId,    fun ?MODULE:sessionid/1},
         {version,   fun ?MODULE:version/1},
         {logLevel,  fun ?MODULE:logLevel/1},
         cmd,
         {args,      fun ?MODULE:args/1},
         cmd_resp,
         {bytes,     fun ?MODULE:bytes/1},
         {time,      fun ?MODULE:time/1},
         connUser,
         connTarget,
         connDbType,
         connStr,
         {sql,       fun ?MODULE:sql/1}]).

src(Access) ->
    case maps:get(src, Access, "") of
        SrcIp when is_tuple(SrcIp) -> inet:ntoa(SrcIp);
        SrcIp when is_list(SrcIp) -> SrcIp;
        SrcIp -> io_lib:format("~p", [SrcIp])
    end.

proxy(Access) ->
    SrcIp = maps:get(src, Access, ""),
    case maps:get(proxy, Access, "") of
        SrcIp -> "yes";
        _ -> "no"
    end.

userid(Access) ->
    case maps:get(userId, Access, "") of
        undefined -> "";
        User when is_list(User) -> User;
        User -> io_lib:format("~p", [User])
    end.

username(Access) ->
    case maps:get(userName, Access, "") of
        User when is_list(User) -> User;
        User when is_binary(User) -> binary_to_list(User);
        User -> io_lib:format("~p", [User])
    end.

sessionid(Access) ->
    base64:encode_to_string(
      integer_to_list(
        erlang:phash2(
          maps:get(sessId, Access, "")))).

version(#{app := App}) ->
    case application:get_key(App, vsn) of
        {ok, Vsn} -> Vsn;
        _ -> ""
    end;
version(_Access) -> "".

args(#{actLogLevel := ActLogLevel} = Access) when ActLogLevel >= ?CMD_WITHARGS ->
    Args = case maps:get(args, Access, "") of
        ArgsBin when is_binary(ArgsBin) ->
            case catch imem_json:decode(ArgsBin, [return_maps]) of
                ArgsMap when is_map(ArgsMap) -> ArgsMap;
                _Error -> ArgsBin
            end;
        AccessArgs -> AccessArgs
    end,
    binary_to_list(
      case Args of
          "" -> <<>>;
          Args when is_binary(Args)  -> Args;
          #{<<"Password">> := _} ->
              imem_json:encode(Args#{<<"Password">> => <<"****">>});
          #{<<"password">> := _, <<"new_password">> := _} ->
              imem_json:encode(Args#{<<"password">> => <<"****">>,
                               <<"new_password">> => <<"****">>});
          #{<<"password">> := _} ->
              imem_json:encode(Args#{<<"password">> => <<"****">>});
          #{<<"qstr">> := _} ->
              imem_json:encode(maps:remove(<<"qstr">>, Args));
          #{<<"query">> := #{<<"qstr">> := _} = Query} ->
              imem_json:encode(Args#{<<"query">> => maps:remove(<<"qstr">>, Query)});
          Args when is_map(Args) -> imem_json:encode(Args);
          Args -> list_to_binary(io_lib:format("~p", [Args]))
      end);
args(_Access) -> "".

bytes(Access) ->
    case maps:get(bytes, Access, "") of
        "" -> "";
        Bytes when is_integer(Bytes) -> integer_to_list(Bytes);
        Bytes -> io_lib:format("~p", [Bytes])
    end.

time(Access) ->
    case maps:get(time, Access, "") of
        "" -> "";
        Time when is_integer(Time) -> integer_to_list(Time);
        Time -> io_lib:format("~p", [Time])
    end.

sql(#{actLogLevel := ActLogLevel} = Access) when ActLogLevel >= ?CUST_SQL ->
    case maps:get(args, Access, "") of
        #{<<"qstr">> := QStr} -> replace_password(QStr);
        #{<<"query">> := #{<<"qstr">> := QStr}} -> replace_password(QStr);
        _ -> ""
    end;
sql(_Access) -> "".

replace_password(QStr) ->
    re:replace(re:replace(QStr, "\n", "\\\\n", [{return, binary}, global]),
      <<"((?i)IDENTIFIED[\\s]+BY[\\s]+)(([^\" ]+)|(\"[^\"]+\"))(.*)">>,
      "\\1****\\5", [{return,binary}]).

logLevel(#{logLevel := LogLevel}) when is_integer(LogLevel) ->
    integer_to_list(LogLevel);
logLevel(_) -> "".

log(Log) -> log(dderl, Log, ?AccessSchema, fun log_fun/1).

log(App, #{logLevel := LogLevel} = Log, Props, LogFun) ->
    ActLogLevel = ?ACTLOGLEVEL(App),
    case ActLogLevel >= LogLevel of
        true ->
            LogFun(Log#{actLogLevel => ActLogLevel, props => Props});
        _ -> ok
    end;
log(_, _, _, _) -> ok.

log_fun(Log) -> access:info(Log).

-spec format(lager_msg:lager_msg(), list(), list()) -> any().
format(Msg, _Config, _Colors) ->
    {Date, Time} = lager_msg:datetime(Msg),
    case lager_msg:message(Msg) of
        #{props := Props} = M ->
            LogParts =
            lists:reverse(
              lists:foldl(
                fun({_Prop, Fun}, Acc) when is_function(Fun) ->
                        case catch Fun(M) of
                            {'EXIT', _Error} -> Acc;
                            V -> [V, ";" | Acc]
                        end;
                   (Prop, Acc) -> [maps:get(Prop, M, ""), ";" | Acc]
                end, [], Props)),
            [Date, " ", Time, ";", atom_to_list(node()), ";", LogParts, "\r\n"];
        M -> [Date, " ", Time, ";", io_lib:format("~p", [M]), "\r\n"]
    end.

-spec format(lager_msg:lager_msg(), list()) -> any().
format(Msg, Config) ->
    format(Msg, Config, []).
