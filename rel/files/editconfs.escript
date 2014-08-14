#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname build_msi -mnesia debug verbose
-include_lib("kernel/include/file.hrl").

-define(L(__F),     io:format(FileHandle, "[~p] "__F"~n", [?LINE])).
-define(L(__F,__A), io:format(FileHandle, "[~p] "__F"~n", [?LINE|__A])).

main(Options) ->
    ScriptFile = escript:script_name(),
    ScriptPath = filename:dirname(ScriptFile),
    FileName = filename:join([ScriptPath
                              , filename:basename(
                                  ScriptFile
                                  , filename:extension(
                                      ScriptFile)
                                 ) ++".log"]),
    {ok, FileHandle} = file:open(FileName, [write]),
    try
        ?L("ScriptPath   : ~s", [ScriptPath]),
        ?L("FileName     : ~s", [FileName]),
        unsafe(FileHandle, Options)
    catch
        Class:Error ->
            ?L("Execution error~n"
               "    Class  : ~p~n"
               "    Error  : ~p~n"
               "    Args   : ~p~n"
               "    Stack  : ~p"
               , [Class,Error,Options,erlang:get_stacktrace()])
    after
        ok = file:close(FileHandle)
    end.

unsafe(FileHandle, Options) ->
    if length(Options) < 9 ->
           ?L("Invalid parameters~n~p~n"
              "Args  dderl_node~n"
              "      dderl_cookie~n"
              "      dderl_ip_port~n"
              "      imem_mnesia_node_type~n"
              "      imem_mnesia_schema_name~n"
              "      imem_erl_cluster_mgrs~n"
              "      imem_ip_port~n"
              "      config_folder~n"
              "      app_data_folder"
              , [Options]);
       true ->
           [DDerlNode, DDerlCookie, DDerlIpPort, ImemNodeType, ImemSchemaName
            , ImemClusterMgrs, ImemIpPort, ConfigFolder, AppDataFolder]
           = Options,
       ?L("starting configure..."),
       update_vm_args(FileHandle, ConfigFolder, DDerlNode, DDerlCookie),
       update_sys_config(FileHandle, ConfigFolder
                         , DDerlIpPort, ImemNodeType, ImemSchemaName
                         , ImemClusterMgrs, ImemIpPort, AppDataFolder),
       ?L("configuring success!")
    end.

update_vm_args(FileHandle, ConfigFolder, DDerlNode, DDerlCookie) ->
    ?L("editing vm.args"),
    ?L("Args -~n"
       "    Path        : ~s~n"
       "    DDerlNode   : ~s~n"
       "    DDerlCookie : ~s"
       , [ConfigFolder, DDerlNode, DDerlCookie]),
    throw("Bad Exception").

update_sys_config(FileHandle, ConfigFolder
                  , DDerlIpPort, ImemNodeType, ImemSchemaName
                  , ImemClusterMgrs, ImemIpPort, AppDataFolder) ->
    ?L("editing sys.config"),
    ?L("Args -~n"
       "    Path            : ~s~n"
       "    DDerlIpPort     : ~s~n"
       "    ImemNodeType    : ~s~n"
       "    ImemSchemaName  : ~s~n"
       "    ImemClusterMgrs : ~s~n"
       "    ImemIpPort      : ~s~n"
       "    AppDataFolder   : ~s"
       , [ConfigFolder, DDerlIpPort, ImemNodeType, ImemSchemaName
          , ImemClusterMgrs, ImemIpPort, AppDataFolder]).
