#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname build_msi -mnesia debug verbose
-include_lib("kernel/include/file.hrl").


% Sample Parameters
% ../dderl/bin/editconfs.escript dderl@127.0.0.1 dderlc 127.0.0.1:8443 ram dderlstag ['dderl@127.0.0.1'] 127.0.0.1:1234 ../dderl/releases/1.0.7 ../dderl/log

-define(T, "").
%-define(T, dtfstr()).
%dtfstr() ->
%    {Y,M,D} = erlang:date(),
%    {H,Min,S} = erlang:time(),
%    lists:flatten(
%      io_lib:format("~4..0B.~2..0B.~2..0B "
%                    "~2..0B:~2..0B:~2..0B "
%                    , [Y,M,D,H,Min,S])).

-define(L(__F),     io:format(FileHandle, ?T++"[~p] "__F"~n", [?LINE])).
-define(L(__F,__A), io:format(FileHandle, ?T++"[~p] "__F"~n", [?LINE|__A])).
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
    update_file(FileHandle, ConfigFolder, "vm.args"
                , [{"-name"
                    % TODO: Check REGEX for all valid nodename
                    , "(.*)(-name )([\.A-Za-z0-9@\-]*)(.*)$"
                    , DDerlNode}
                   , {"-setcookie"
                      % TODO: Update REGEX for all valid erlang atoms
                      , "(.*)(-setcookie )([\.A-Za-z0-9@_\-]*)(.*)$"
                      , DDerlCookie}
                  ]),
    update_file(FileHandle, ConfigFolder, "vm_t.args"
                , [{"-setcookie"
                    % TODO: Update REGEX for all valid erlang atoms
                    , "(.*)(-setcookie )([\.A-Za-z0-9@_\-]*)(.*)$"
                    , DDerlCookie}
                  ]).

update_sys_config(FileHandle, ConfigFolder
                  , DDerlIpPort, ImemNodeType, ImemSchemaName
                  , ImemClusterMgrs, ImemIpPort, AppDataFolder) ->
    ?L("editing sys.config"),
    ?L("Args (Input) -~n"
       "    Path            : ~p~n"
       "    DDerlIpPort     : ~p~n"
       "    ImemNodeType    : ~p~n"
       "    ImemSchemaName  : ~p~n"
       "    ImemClusterMgrs : ~p~n"
       "    ImemIpPort      : ~p~n"
       "    AppDataFolder   : ~p"
       , [ConfigFolder, DDerlIpPort, ImemNodeType, ImemSchemaName
          , ImemClusterMgrs, ImemIpPort, AppDataFolder]),
    {DDerlHost, DDerlPort} = case re:run(DDerlIpPort
                , "([^:]*):([0-9]*)"
                , [{capture, [1,2], list}]) of
        {match, [DDHost,DDPort]} ->
            {DDHost, list_to_integer(DDPort)};
        nomatch -> {undefined, undefined}
    end,
    {ImemHost, ImemPort} = case re:run(ImemIpPort
                , "([^:]*):([0-9]*)"
                , [{capture, [1,2], list}]) of
        {match, [IHost,IPort]} ->
            {IHost, list_to_integer(IPort)};
        nomatch -> {undefined, undefined}
    end,
    ImemNodeTypeAtom = list_to_atom(ImemNodeType),
    ImemSchemaNameAtom = list_to_atom(ImemSchemaName),
    {ok, ImemClusterMgrsToks, _} = erl_scan:string(ImemClusterMgrs++".",0,[]),
    {ok, ImemClusterMgrsTerm} = erl_parse:parse_term(ImemClusterMgrsToks),
    ?L("Args (Processed) -~n"
       "    Path                : ~p~n"
       "    DDerlHost           : ~p~n"
       "    DDerlPort           : ~p~n"
       "    ImemNodeTypeAtom    : ~p~n"
       "    ImemSchemaNameAtom  : ~p~n"
       "    ImemClusterMgrsTerm : ~p~n"
       "    ImemHost            : ~p~n"
       "    ImemPort            : ~p~n"
       "    AppDataFolder       : ~p"
       , [ConfigFolder, DDerlHost, DDerlPort, ImemNodeTypeAtom, ImemSchemaNameAtom
          , ImemClusterMgrsTerm, ImemHost, ImemPort, AppDataFolder]),
    SnapDir = filename:join([AppDataFolder, "snapshot"]),
    ErrorLog = filename:join([AppDataFolder, "log", "error.log"]),
    ConsoleLog = filename:join([AppDataFolder, "log", "console.log"]),
    CrashLog = filename:join([AppDataFolder, "log", "crash.log"]),
    update_file_term(FileHandle, ConfigFolder, "sys.config",
                    [{[dderl, interface], [], DDerlHost}
                     , {[dderl, port], [], DDerlPort}
                     , {[imem, mnesia_node_type], [], ImemNodeTypeAtom}
                     , {[imem, mnesia_schema_name], [], ImemSchemaNameAtom}
                     , {[imem, erl_cluster_mgrs], [], ImemClusterMgrsTerm}
                     , {[imem, tcp_ip], [], ImemHost}
                     , {[imem, tcp_port], [], ImemPort}
                     , {[imem, imem_snapshot_dir], [], SnapDir}
                     , {[lager, handlers, lager_file_backend, file]
                        , [{level, error}], ErrorLog}
                     , {[lager, handlers, lager_file_backend, file]
                        , [{level, info}], ConsoleLog}
                     , {[lager, crash_log], [], CrashLog}
                    ]).

update_file_term(FileHandle, ConfigFolder, File, Configs) ->
    FilePath = filename:join(ConfigFolder, File),
    case file:read_file(FilePath) of
        {ok, FileBin} ->
            ?L("{~s} read success from ~s", [File, FilePath]),
            {ok, TClean, _} = erl_scan:string(
                                binary_to_list(FileBin),0,[]),
            {ok, Tok, _} = erl_scan:string(
                             binary_to_list(FileBin),0,[return]),
            {ok, Pt} = erl_parse:parse_term(TClean),
            NewPt = modify_nested_proplist(FileHandle, File, Pt, Configs),
            NewSc = lists:flatten(io_lib:format("~p.", [NewPt])),
            %?L("{~s} before ->~n~p", [File, Pt]),
            %?L("{~s} after ->~n~p", [File, NewPt]),
            {ok, TNewSc, _} = erl_scan:string(NewSc,0,[]),
            TMerge = merge_toks(TNewSc, Tok),
            NewFileStr = build_string(TMerge),
            FileStr = binary_to_list(FileBin),
            if NewFileStr =/= FileStr ->
                   {ok, _} = file:copy(
                               FilePath
                               , filename:join(ConfigFolder
                                               , File ++ "."
                                               ++ dtstr())),
                   ok = file:write_file(FilePath, NewFileStr);
               true ->
                   ?L("{~s} unmodified at ~s", [File, FilePath])
            end;
        {error, Reason} ->
            ?L("{~s}~n"
               "    from        : ~s~n"
               "    read error  : ~p"
               , [File, FilePath, Reason])
    end.

modify_nested_proplist(_FileHandle, _File, NewTerm, []) -> NewTerm;
modify_nested_proplist(FileHandle, File, Term, {[P], Match, Change}) ->
    MatchFun = fun({M,V}, A) ->
        A andalso
        case proplists:get_value(M,Term) of
            undefined -> false;
            V -> true;
            _ -> false
        end
    end,
    FoldFun =
    fun(OldTerm,Acc) ->
        case OldTerm of
            {P,SubTerm} ->
                case lists:foldl(MatchFun, true, Match) of
                    true ->
                        ?L("{~s} changing ~p ~p -> ~p"
                           , [File, P, SubTerm, Change]),
                        [{P, Change} | Acc];
                    false -> [OldTerm | Acc]
                end;
            OldTerm ->
                [OldTerm | Acc]
        end
    end,
    lists:reverse(lists:foldl(FoldFun, [], Term));
modify_nested_proplist(FileHandle, File, Term, {[P|Path], Match, Change}) ->
    lists:reverse(
      lists:foldl(
        fun(OldTerm,A) ->
                case OldTerm of
                    {P,SubTerm} ->
                        NewSubTerm =
                        modify_nested_proplist(
                          FileHandle, File, SubTerm
                          , {Path, Match, Change}),
                        [{P, NewSubTerm} | A];
                    OldTerm ->
                        [OldTerm | A]
                end
        end
        , [], Term));
modify_nested_proplist(FileHandle, File, Term, [{Path, Match, Change}|Config]) ->
    ?L("{~s} at ~s", [File, string:join([atom_to_list(P) || P <- Path], "/")]),
    NewTerm = modify_nested_proplist(FileHandle, File, Term, {Path, Match, Change}),
    modify_nested_proplist(FileHandle, File, NewTerm, Config).

build_string(TMerge) ->
    build_string(TMerge, "").
build_string([], Str) -> Str;
build_string([Tok|T], Str) ->
    NewStr = Str
    ++ case Tok of
           {white_space, _, WS}     -> WS;
           {comment, _, Cmt}        -> Cmt;
           {string,_,S}             -> "\""++S++"\"";
           {atom, _, A}             -> lists:flatten(io_lib:format("~p",[A]));
           {char, _, C}             -> [C];
           {float, _, F}            -> float_to_list(F);
           {integer, _, I}          -> integer_to_list(I);
           {var, _, V}              -> lists:flatten(io_lib:format("~p",[V]));
           {dot, _}                 -> ".";
           {Pc, _} when is_atom(Pc) -> atom_to_list(Pc)
    end,
    build_string(T, NewStr).

merge_toks(T1, T2) ->
    merge_toks(T1, T2, []).
merge_toks([], [], Acc) -> lists:reverse(Acc);
merge_toks([{A,_} = T|R1], [{B,_} = T1|R2], Acc) ->
    if
        A =:= B ->
            %io:format("[~p] ~p -> ~p~n", [?LINE, T, T1]),
            merge_toks(R1,R2,[T|Acc]);
        (A =:= ',') andalso (B =/= ',') ->
            %io:format("[~p] shift R1 ~p -> ~p~n", [?LINE, T, T1]),
            merge_toks(R1,[T1|R2],[T|Acc]);
        (A =/= ',') andalso (B =:= ',') ->
            %io:format("[~p] red R2 ~p -> ~p~n", [?LINE, T, T1]),
            merge_toks([T|R1],R2,Acc);
        true ->
            %io:format("[~p] ERROR ~p -> ~p~n", [?LINE, T, T1]),
            throw({error, marge_failed, {?LINE, [T|R1], [T1|R2], Acc}})
    end;
merge_toks(R1, [{white_space,_,_} = ET|R2], Acc) ->
    merge_toks(R1,R2,[ET|Acc]);
merge_toks(R1, [{comment,_,_} = ET|R2], Acc) ->
    merge_toks(R1,R2,[ET|Acc]);
merge_toks([{_,_} = _T|_] = R1, [{_,_,_} = _T1|R2], Acc) ->
    %io:format("[~p] red R2 ~p -> ~p~n", [?LINE, T, T1]),
    merge_toks(R1,R2,Acc);
merge_toks([{_,_,_} = T|R1], [{_,_} = _T1|_] = R2, Acc) ->
    %io:format("[~p] shift R1 ~p -> ~p~n", [?LINE, T, T1]),
    merge_toks(R1,R2,[T|Acc]);
merge_toks([{A,_,_} = T|R1], [{A,_,_} = _T1|R2], Acc) ->
    %io:format("[~p] ~p -> ~p~n", [?LINE, T, T1]),
    merge_toks(R1,R2,[T|Acc]);
merge_toks(R1, R2, Acc) ->
    %io:format("Bailing...~n", []),
    %io:format("R1 = ~n~p~n", [R1]),
    %io:format("R2 = ~n~p~n", [R2]),
    %io:format("Acc = ~n~p~n", [lists:reverse(Acc)]),
    throw({error, marge_failed, {?LINE, R1, R2, Acc}}).

update_file(FileHandle, ConfigFolder, File, Changes) ->
    FilePath = filename:join(ConfigFolder, File),
    case file:read_file(FilePath) of
        {ok, FileBin} ->
            ?L("{~s} read success from ~s", [File, FilePath]),
            %io:format(user, "{~s}~n~s~n", [File, FileBin]),
            NewFileBin =
            bin_replace(FileHandle, File, FileBin, Changes),
            if NewFileBin =/= FileBin ->
                   {ok, _} = file:copy(FilePath
                                       , filename:join(ConfigFolder
                                                       , File ++ "."
                                                       ++ dtstr())),
                   ok = file:write_file(FilePath, NewFileBin);
               true ->
                   ?L("{~s} unmodified at ~s", [File, FilePath])
            end;
        {error, Reason} ->
            ?L("{~s}~n"
               "    from        : ~s~n"
               "    read error  : ~p"
               , [File, FilePath, Reason])
    end.

bin_replace(_, _, ModifiedBin, []) -> ModifiedBin;
bin_replace(FileHandle, File, TargetBin, [{Name,Re,NewVal}|Rest]) ->
    case re:run(TargetBin, Re, [{capture, [1,2,3,4], list},dotall]) of
        {match, [Front,Tag,OldVal,Tail]} ->
            ?L("{~s} changing ~s ~s -> ~s", [File, Name, OldVal, NewVal]),
            bin_replace(FileHandle, File
                        , list_to_binary([Front,Tag,NewVal,Tail]), Rest);
        nomatch ->
            ?L("{~s} ~s not found in ~s", [File, Name]),
            bin_replace(FileHandle, File, TargetBin, Rest)
    end.

dtstr() ->
    {Y,M,D} = erlang:date(),
    {H,Min,S} = erlang:time(),
    lists:flatten(
      io_lib:format("~4..0B~2..0B~2..0B"
                    "~2..0B~2..0B~2..0B"
                    , [Y,M,D,H,Min,S])).

