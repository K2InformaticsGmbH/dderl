-module(dderl_access_logger).
-include("dderl.hrl").

-export([log/2, log/5]).

% library APIs
-export([src/1, proxy/1, userid/1, username/1, sessionid/1, bytes/1, time/1,
         args/1, sql/1, version/1]).

-define(AccessSchema,
        [{src,       fun dderl_access_logger:src/1},
         {proxy,     fun dderl_access_logger:proxy/1},
         {userId,    fun dderl_access_logger:userid/1},
         {userName,  fun dderl_access_logger:username/1},
         {sessId,    fun dderl_access_logger:sessionid/1},
         {version,   fun dderl_access_logger:version/1},
         loglevel,
         cmd,
         {args,      fun dderl_access_logger:args/1},
         {bytes,     fun dderl_access_logger:bytes/1},
         {time,      fun dderl_access_logger:time/1},
         connUser,
         connTarget,
         connDbType,
         connStr,
         {sql,       fun dderl_access_logger:sql/1}]).

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

args(Access) ->
    CmdArgs = maps:get(args, Access, ""),
    binary_to_list(
      case CmdArgs of
          "" -> <<>>;
          CmdArgs when is_binary(CmdArgs)  -> CmdArgs;
          #{<<"Password">> := _} ->
              jsx:encode(CmdArgs#{<<"Password">> => <<"****">>});
          #{<<"password">> := _} ->
              jsx:encode(CmdArgs#{<<"password">> => <<"****">>});
          #{<<"password">> := _, <<"new_password">> := _} ->
              jsx:encode(CmdArgs#{<<"password">> => <<"****">>,
                                  <<"new_password">> => <<"****">>});
          #{<<"qstr">> := _} ->
              jsx:encode(maps:remove(<<"qstr">>, CmdArgs));
          CmdArgs when is_map(CmdArgs) -> jsx:encode(CmdArgs);
          CmdArgs -> list_to_binary(io_lib:format("~p", [CmdArgs]))
      end).

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

sql(Access) ->
    case maps:get(args, Access, "") of
        #{<<"qstr">> := QStr} ->
            re:replace(
              QStr, <<"((?i)IDENTIFIED[\\s]+BY[\\s]+)(([^\" ]+)|(\"[^\"]+\"))(.*)">>,
              "\\1****\\5", [{return,binary}]);
        _ -> ""
    end.

log(LogLevel, Log) -> log(dderl, LogLevel, Log, ?AccessSchema, fun log_fun/1).

log(App, LogLevel, Log, Props, LogFun) ->
    case ?ACTLOGLEVEL(App) >= LogLevel of
        true -> 
            LogMsg = msg_str(Log, Props),
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
