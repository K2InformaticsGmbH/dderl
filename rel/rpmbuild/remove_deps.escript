#!/usr/bin/env escript
%% -*- mode: erlang -*-
%% ex: ft=erlang
%%! -smp enable -sname build_rpm -mnesia debug verbose
-include_lib("kernel/include/file.hrl").

-define(OSCMD(__Cmd),
    (fun() ->
        CR = os:cmd(__Cmd),
        CmdResp = case lists:reverse(CR) of
            [$\r,$\n|Rest] -> lists:reverse(Rest);
            [$\n,$\r|Rest] -> lists:reverse(Rest);
            [$\n|Rest] -> lists:reverse(Rest);
            [$\r|Rest] -> lists:reverse(Rest);
            _ -> CR
        end,
        io:format("[~p] "++__Cmd++": ~s~n", [?LINE, CmdResp]),
        CmdResp
    end)()
).

-define(L(__Fmt,__Args), io:format("[~p] "++__Fmt++"~n", [?LINE | __Args])).
-define(L(__Fmt), ?L(__Fmt,[])).

main(Deps) ->
    Pwd = filename:dirname(filename:absname(escript:script_name())),
    [_,_|ProjectDirParts] = lists:reverse(filename:split(Pwd)),
    ConfFile = filename:join(lists:reverse(ProjectDirParts) ++ ["rebar.config"]),
    {ok, Conf} = file:consult(ConfFile),
    main(ConfFile, Conf, Conf, Deps).

main(ConfFile, Conf, Conf, []) ->
    ?L("Unmodified Configuration ~p", [ConfFile]);
main(ConfFile, Conf, OrigConf, []) ->
    ?L("Config file ~p", [ConfFile]),
    ?L("Original content:~n~p~n", [OrigConf]),
    ?L("The new conf:~n~p~n", [Conf]),
    ConfBin = iolist_to_binary([io_lib:format("~p.~n", [C]) || C <- Conf]),
    ok = file:write_file(ConfFile, ConfBin);
main(ConfFile, Conf, OrigConf, [Dep| Deps]) ->
    main(ConfFile, remove_dep(Conf, Dep), OrigConf, Deps).

remove_dep(Conf, Dep) ->
    Deps = proplists:get_value(deps, Conf),
    lists:keyreplace(deps, 1, Conf, {deps, [D || D <- Deps, atom_to_list(element(1,D)) =/= Dep]}).
