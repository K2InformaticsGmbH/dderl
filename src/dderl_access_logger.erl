-module(dderl_access_logger).
-include("dderl.hrl").

-export([install/0, install/2, uninstall/0, uninstall/1, log/2, log/4]).

% library APIs
-export([src/1, proxy/1, userid/1, username/1, sessionid/1, bytes/1, time/1,
         args/1, sql/1, version/1]).

-define(AccessSchema,
        [{src,       fun ?MODULE:src/1},
         {proxy,     fun ?MODULE:proxy/1},
         {userId,    fun ?MODULE:userid/1},
         {userName,  fun ?MODULE:username/1},
         {sessId,    fun ?MODULE:sessionid/1},
         {version,   fun ?MODULE:version/1},
         loglevel,
         cmd,
         {args,      fun ?MODULE:args/1},
         {bytes,     fun ?MODULE:bytes/1},
         {time,      fun ?MODULE:time/1},
         connUser,
         connTarget,
         connDbType,
         connStr,
         {sql,       fun ?MODULE:sql/1}]).

install() -> install(dderl, ?AccessSchema).
install(App, AccessSchema) ->
    LogFile = lists:concat(["log/",App,"_access.log"]),
    ok = gen_event:add_handler(
           lager_event, {dderl_access_lager_file_backend, App},
           [{file, LogFile}, {level, debug}, {size, 10485760},
            {date, "$D0"}, {count, 5}, {application, App},
            {props, AccessSchema}]),
    ok = lager:set_loglevel({dderl_access_lager_file_backend, App}, debug),
    ?Info("~p activity logger started", [App]).

uninstall() -> uninstall(dderl).
uninstall(App) ->
    ok = gen_event:delete_handler(
           lager_event, {dderl_access_lager_file_backend, App}, []).

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

version(Access) ->
    case Access of
        #{version := Vsn} -> Vsn;
        #{app := App} ->
            case application:get_key(App, vsn) of
                {ok, Vsn} -> Vsn;
                _ -> ""
            end;
        _ -> ""
    end.

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

log(LogLevel, Log) -> log(dderl, LogLevel, Log, fun log_low/1).
log(App, LogLevel, Log, LogFun) ->
    case ?ACTLOGLEVEL(App) >= LogLevel of
       true ->
            LogFun(Log#{proxy => ?PROXY,
                        loglevel => integer_to_list(LogLevel)});
        _ -> ok
    end.
% MUST log through local function, module/app filter in access logger for routing
log_low(Log) -> lager:debug([{type,dderl_access}], Log).

