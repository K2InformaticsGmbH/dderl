#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname build_msi -mnesia debug verbose

main([DDerlNode, DDerlCookie, DDerlIpPort
      , ImemNodeType, ImemSchemaName, ImemClusterMgrs, ImemIpPort
      , AppDataFolder, PersonalFolder]) ->
    ScriptPath = filename:dirname(escript:script_name()),
    Content = lists:flatten(
                io_lib:format("ScriptPath ~p Configs ~p"
                              , [ScriptPath
                                , {DDerlNode, DDerlCookie
                                   , DDerlIpPort, ImemNodeType
                                   , ImemSchemaName, ImemClusterMgrs
                                   , ImemIpPort, AppDataFolder
                                   , PersonalFolder}])),
    FileName = filename:join([ScriptPath, "test.log"]),
    ok = file:write_file(FileName, Content);
main(Others) ->
    ScriptPath = filename:dirname(escript:script_name()),
    FileName = filename:join([ScriptPath, "test.log"]),
    Content = lists:flatten(io_lib:format("Invalid parameters ~p~n"
              "Usage ~s dderl_node dderl_cookie dderl_ip_port~n"
              "         imem_mnesia_node_type imem_mnesia_schema_name~n"
              "         imem_erl_cluster_mgrs imem_ip_port app_data_folder~n"
              "         personal_folder~n"
              , [Others, escript:script_name()])),
    ok = file:write_file(FileName, Content).
