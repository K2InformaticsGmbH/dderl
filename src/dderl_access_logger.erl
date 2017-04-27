-module(dderl_access_logger).
-include("dderl.hrl").

-export([install/1, uninstall/1, log/2]).

-define(AccessSchema,
        [{src,       fun access_src/1},
         {proxy,     fun access_proxy/1},
         {userId,    fun access_userid/1},
         {userName,  fun access_username/1},
         {sessId,    fun access_sessionid/1},
         {version,   fun access_version/1},
         loglevel,
         cmd,
         {args,      fun access_args/1},
         {bytes,     fun access_bytes/1},
         {time,      fun access_time/1},
         connUser,
         connTarget,
         connDbType,
         connStr,
         {sql,       fun access_sql/1}]).


install(App) ->
    ok = gen_event:add_handler(
           lager_event, {dderl_access_lager_file_backend, App},
           [{file, "log/dderl_access.log"}, {level, debug}, {size, 10485760},
            {date, "$D0"}, {count, 5}, {application, App}, {props, ?AccessSchema}]),
    ok = lager:set_loglevel({dderl_access_lager_file_backend, App}, debug),
    ?Info("activity logger started").

uninstall(App) ->
    ok = gen_event:delete_handler(
           lager_event, {dderl_access_lager_file_backend, App}, []).

access_src(Access) ->
    case maps:get(src, Access, "") of
        SrcIp when is_tuple(SrcIp) -> inet:ntoa(SrcIp);
        SrcIp when is_list(SrcIp) -> SrcIp;
        SrcIp -> io_lib:format("~p", [SrcIp])
    end.

access_proxy(Access) ->
    SrcIp = maps:get(src, Access, ""),
    case maps:get(proxy, Access, "") of
        SrcIp -> "yes";
        _ -> "no"
    end.

access_userid(Access) ->
    case maps:get(userId, Access, "") of
        undefined -> "";
        User when is_list(User) -> User;
        User -> io_lib:format("~p", [User])
    end.

access_username(Access) ->
    case maps:get(userName, Access, "") of
        User when is_list(User) -> User;
        User when is_binary(User) -> binary_to_list(User);
        User -> io_lib:format("~p", [User])
    end.

access_sessionid(Access) ->
    base64:encode_to_string(
      integer_to_list(
        erlang:phash2(
          maps:get(sessId, Access, "")))).

access_version(Access) ->
    case application:get_key(maps:get(app,Access,undefined)) of
        {ok, Vsn} -> Vsn;
        _ -> ""
    end.

access_args(Access) ->
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

access_bytes(Access) ->
    case maps:get(bytes, Access, "") of
        "" -> "";
        Bytes when is_integer(Bytes) -> integer_to_list(Bytes);
        Bytes -> io_lib:format("~p", [Bytes])
    end.

access_time(Access) ->
    case maps:get(time, Access, "") of
        "" -> "";
        Time when is_integer(Time) -> integer_to_list(Time);
        Time -> io_lib:format("~p", [Time])
    end.


access_sql(Access) ->
    case maps:get(args, Access, "") of
        #{<<"qstr">> := QStr} ->
            re:replace(
              QStr, <<"((?i)IDENTIFIED[\\s]+BY[\\s]+)(([^\" ]+)|(\"[^\"]+\"))(.*)">>,
              "\\1****\\5", [{return,binary}]);
        _ -> ""
    end.

log(LogLevel, #{app := App} = Log) ->
    case ?ACTLOGLEVEL(App) >= LogLevel of
       true ->
            lager:debug(
              [{type,dderl_access}],
              Log#{proxy => ?PROXY, loglevel => integer_to_list(LogLevel)});
        _ -> ok
    end.
