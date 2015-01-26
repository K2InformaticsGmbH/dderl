#!/usr/bin/env escript
%% -*- mode: erlang -*-
%% ex: ft=erlang
%%! -smp enable -kernel inet_dist_listen_min 7000 -kernel inet_dist_listen_max 7020

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

main([Node,Host,CookieStr]) ->
    NodeName = list_to_atom(lists:flatten([Node,"_script@",Host])),
    RemoteNode = list_to_atom(lists:flatten([Node,"@",Host])),
    Cookie = list_to_atom(CookieStr),
    {ok, Pid} = net_kernel:start([NodeName, longnames]),
    true = erlang:set_cookie(node(), Cookie),
    pong = net_adm:ping(RemoteNode),
    ?L("Started node ~p, process ~p", [node(), Pid]),
    {Time, _} = rpc:call(RemoteNode, timer, tc, [remote_fun()]),
    ?L("Executed in ~pms", [Time]).

remote_fun() ->
    ScriptNode = node(),
    fun() ->
            ?L("Erlang cluster nodes : ~p", [[node()|nodes()]--[ScriptNode]]),
            ?L("IMEM cluster nodes : ~p", [imem_meta:data_nodes()])
    end.
