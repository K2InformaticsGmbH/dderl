-module(dderl_access_logger).
-include("dderl.hrl").

-export([log/2, log/5]).

% library APIs
-export([src/1, proxy/1, userid/1, username/1, sessionid/1, bytes/1, time/1,
         args/1, sql/1, version/1, logLevel/1]).

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

args(#{appLogLevel := AppLogLevel} = Access) when AppLogLevel >= ?CMD_WITHARGS ->
    Args = maps:get(args, Access, ""),
    binary_to_list(
      case Args of
          "" -> <<>>;
          Args when is_binary(Args)  -> Args;
          #{<<"Password">> := _} ->
              jsx:encode(Args#{<<"Password">> => <<"****">>});
          #{<<"password">> := _} ->
              jsx:encode(Args#{<<"password">> => <<"****">>});
          #{<<"password">> := _, <<"new_password">> := _} ->
              jsx:encode(Args#{<<"password">> => <<"****">>,
                                  <<"new_password">> => <<"****">>});
          #{<<"qstr">> := _} ->
              jsx:encode(maps:remove(<<"qstr">>, Args));
          Args when is_map(Args) -> jsx:encode(Args);
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

sql(#{appLogLevel := AppLogLevel} = Access) when AppLogLevel >= ?CUST_SQL ->
    case maps:get(args, Access, "") of
        #{<<"qstr">> := QStr} ->
            re:replace(
              QStr, <<"((?i)IDENTIFIED[\\s]+BY[\\s]+)(([^\" ]+)|(\"[^\"]+\"))(.*)">>,
              "\\1****\\5", [{return,binary}]);
        _ -> ""
    end;
sql(_Access) -> "".

logLevel(#{logLevel := LogLevel}) when is_integer(LogLevel) ->
    integer_to_list(LogLevel);
logLevel(_) -> "".

log(LogLevel, Log) -> log(dderl, LogLevel, Log, ?AccessSchema, fun log_fun/1).

log(App, LogLevel, Log, Props, LogFun) ->
    AppLogLevel = ?ACTLOGLEVEL(App),
    case AppLogLevel >= LogLevel of
        true -> 
            LogMsg = msg_str(Log#{appLogLevel => AppLogLevel}, Props),
            LogFun(LogMsg);
        _ -> ok
    end.

log_fun(Log) -> access:info("~s", [Log]).

msg_str(Msg, Props) ->
    LogParts =
    lists:reverse(
      lists:foldl(
        fun({_Prop, Fun}, Acc) when is_function(Fun) ->
                case catch Fun(Msg) of
                    {'EXIT', _Error} -> Acc;
                    V -> [V, ";" | Acc]
                end;
           (Prop, Acc) -> [maps:get(Prop, Msg, ""), ";" | Acc]
        end, [], Props)),
    lists:flatten([atom_to_list(node()) ++ LogParts]).
