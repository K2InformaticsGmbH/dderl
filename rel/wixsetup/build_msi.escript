#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -mnesia debug verbose
-include_lib("kernel/include/file.hrl").

-define(COMPANY, "K2 Informatics GmbH").
-define(PKG_COMMENT, "DDErl is a registered trademark of"
                     " K2 Informatics GmbH").

-define(H(__F), integer_to_list(erlang:phash2(__F), 16)).

-define(TRACE,  io:format("TRACE ~p~n", [?LINE])).

-record(item, { id
              , type % file | dir | component
              , guid
              , name
              , path
              , file_info
        }).


-define(E(__Fmt,__Args), io:format("[~p] "++__Fmt++"~n", [?LINE | __Args])).
-define(E(__Fmt), ?E(__Fmt,[])).

-define(L(__Fmt,__Args),
        if Verbose ->
               io:format("[~p] "++__Fmt++"~n",
                         [?LINE | __Args]);
           true -> ok
        end).
-define(L(__Fmt), ?L(__Fmt,[])).

-define(OSCMD(__Cmd),
    (fun() ->
        CR = os:cmd(__Cmd),
        CmdResp = re:replace(CR, "[\r\n ]*$", "", [{return, list}]),
        ?L(__Cmd++": ~s", [CmdResp]),
        CmdResp
    end)()
).

main([Proj, ProjDir, ReleaseDir, "-v"]) ->
    build_msi(true, Proj, ProjDir, ReleaseDir);
main([Proj, ProjDir, ReleaseDir]) ->
    ?E("no verbose"),
    build_msi(false, Proj, ProjDir, ReleaseDir);
main(Opts) ->
    ?E("Invalid Opts ~p", [Opts]).

build_msi(Verbose, Proj, ProjDir, ReleaseDir) ->
    {ok, [{application,_,AppProps}]} =
        file:consult(filename:join([ProjDir,"src",Proj++".app.src"])),
    Version = proplists:get_value(vsn, AppProps, ""),
    ?L("Building ~s-~s MSI", [Proj, Version]),
    BuildSourceDir = filename:join(ReleaseDir, Proj++"-"++Version),
    build_sources(Verbose, Proj, ProjDir, BuildSourceDir),
    rebar_generate(Verbose, Proj, BuildSourceDir),
    {ok, _} = dets:open_file(Proj++"ids", [{ram_file, true},
                                           {file, Proj++"ids.dets"},
                                           {keypos, 2}]),
    create_wxs(Verbose, Proj, Version, BuildSourceDir),
    candle_light(Verbose, Proj, Version, BuildSourceDir),
    ok = dets:close(Proj++"ids").
    
uuid() ->
    string:to_upper(re:replace(os:cmd("uuidgen.exe"), "\r\n", "",
                               [{return, list}])).

run_port(Cmd, Args) ->
    log_cmd(Cmd,
            erlang:open_port({spawn_executable, Cmd},
                             [{line, 128},{args, Args}, exit_status,
                              stderr_to_stdout, {parallelism, true}])).
run_port(Cmd, Args, Cwd) ->
    log_cmd(Cmd,
            erlang:open_port({spawn_executable, Cmd},
                             [{cd, Cwd},{line, 128},{args, Args},
                              exit_status,stderr_to_stdout,
                              {parallelism, true}])).

-define(NL(__Fmt,__Args), io:format(__Fmt, __Args)).
-define(NL(__Fmt), ?NL(__Fmt,[])).
log_cmd(Cmd, Port) when is_port(Port) ->
    receive
        {'EXIT',Port,Reason} -> ?E("~s terminated for ~p", [Cmd, Reason]);
        {Port,closed} -> ?E("~s terminated", [Cmd]);
        {Port,{exit_status,Status}} ->
            ?E("~s exit with status ~p", [Cmd, Status]),
            catch erlang:port_close(Port);
        {Port,{data,{F,Line}}} ->
            ?NL("~s" ++ if F =:= eol -> "~n"; true -> "" end, [Line]),
            log_cmd(Cmd, Port);
        {Port,{data,Data}} ->
            ?NL("~p", [Data]),
            log_cmd(Cmd, Port)
    end.

build_sources(Verbose, Proj, ProjDir, RootDir) ->
    ?L("Source ~s", [ProjDir]),
    ?L("Build Source in ~s", [RootDir]),
    ?OSCMD("rm -rf "++RootDir),
    ok = file:make_dir(RootDir),
    [begin
        {ok, _} = file:copy(filename:join(ProjDir,F),
                        filename:join(RootDir,F))
    end || F <- ["rebar.config", "LICENSE", "README.md",
                 "RELEASE-"++string:to_upper(Proj)++".md"]],
    RebarCmd = os:find_executable("rebar"),
    ?OSCMD("cp -L \""++RebarCmd++"\" \""++RootDir++"\""),
    ?OSCMD("cp -L \""++filename:rootname(RebarCmd)++"\" \""++RootDir++"\""),
    ok = file:write_file(filename:join(RootDir,"rebar.bat"),
                         <<"rebar.cmd %*\n">>),
    copy_folder(ProjDir, RootDir, ["include"], "*.*"),
    copy_folder(ProjDir, RootDir, ["src"], "*.*"),
    copy_folder(ProjDir, RootDir, ["docs"], "*.*"),
    copy_folder(ProjDir, RootDir, ["rel"], "*.*"),
    copy_folder(ProjDir, RootDir, ["rel", "files"], "*"),
    copy_folder(ProjDir, RootDir, ["rel", "wixsetup"], "*.*"),

    Priv = filename:join(RootDir, "priv"),
    ok = file:make_dir(Priv),
    copy_deep(filename:join([ProjDir, "priv"]), Priv),
    
    Deps = filename:join(RootDir, "deps"),
    ok = file:make_dir(Deps),
    copy_deep(filename:join([ProjDir, "deps"]), Deps).

copy_folder(Src, Target, Folders, Match) ->
    ok = file:make_dir(filename:join([Target|Folders])),
    SrcFolder = filename:join([Src|Folders]),
    [begin
         Source = filename:join(SrcFolder,F),
         IsFile = (filelib:is_dir(Source) == false andalso
                   filename:extension(Source) /= ".swp"),
         if IsFile ->
                {ok, _} = file:copy(Source,
                                    filename:join([Target|Folders]++[F]));
                true -> ok
         end
     end || F <- filelib:wildcard(Match, SrcFolder)].

copy_deep(ProjDep, TargetDep) ->
    [case D of
        ".git" -> skip;
        "ebin" -> skip;
        ".gitignore" -> skip;       
        D ->
            case filelib:is_dir(filename:join(ProjDep, D)) of
                true ->
                    ok = file:make_dir(filename:join(TargetDep, D)),
                    copy_deep(filename:join(ProjDep, D)
                            , filename:join(TargetDep, D));
                false ->
                    SrcFile = filename:join(ProjDep, D),
                    DstFile = filename:join(TargetDep, D),
                    {ok, #file_info{mode = Mode}} = file:read_file_info(SrcFile),
                    {ok, _} = file:copy(SrcFile, DstFile),
                    ok = file:change_mode(DstFile, Mode)
            end
     end
     || D <- filelib:wildcard("*", ProjDep)].

rebar_generate(Verbose, Proj, Root) ->
    file:delete(Proj++"ids.dets"),
    {ok, CurDir} = file:get_cwd(),
    if Verbose ->
           ?L("Entering ~s from ~s", [Root, CurDir]);
       true -> ok
    end,
    ok = file:set_cwd(Root),
    ?L("Clean Compile and generate..."),
    run_port("rebar.bat", ["clean"], Root),
    run_port("rebar.bat", ["compile"], Root),
    run_port("rebar.bat", ["generate", "skip_deps=true"], Root),
    if Verbose ->
           ?L("Leaving ~s to ~s", [Root, CurDir]);
       true -> ok
    end,
    ok = file:set_cwd(CurDir).

create_wxs(Verbose, Proj, Version, Root) ->
    Tab = Proj++"ids",
    Product = Proj++"_"++Version,
    {ok, FileH} = file:open(
                    filename:join([Root, "rel", "wixsetup",
                                   lists:flatten([Proj,"-",Version,".wxs"])]),
                    [write, raw]),
    {ok, PRODUCT_GUID} = get_id(Verbose, Tab, undefined, 'PRODUCT_GUID', undefined),
    {ok, UPGRADE_GUID} = get_id(Verbose, Tab, undefined, 'UPGRADE_GUID', undefined),
    {ok, ID} = get_id(Verbose, Tab, undefined, ?COMPANY, undefined),
    ok = file:write(FileH,
        "<?xml version='1.0' encoding='windows-1252'?>\n"
        "<Wix xmlns='http://schemas.microsoft.com/wix/2006/wi'>\n\n"

        "<Product Name='"++Product++"'\n"
        "         Id='"++PRODUCT_GUID++"'\n"
        "         UpgradeCode='"++UPGRADE_GUID++"'\n"
        "         Language='1033' Codepage='1252' Version='1.0.7'\n"
        "         Manufacturer='"?COMPANY"'>\n\n"

        "   <Package Id='*'\n"
        "            Keywords='Installer'\n"
        "            Description=\""?COMPANY"\"\n"
        "            Comments='"?PKG_COMMENT"'\n"
        "            Manufacturer='"?COMPANY"'\n"
        "            InstallerVersion='200' Languages='1033'\n"
        "            Compressed='yes'\n"
        "            InstallScope='perMachine'\n"
        "            InstallPrivileges='elevated'\n"
        "            SummaryCodepage='1252' />\n\n"

        "   <Media Id='1' Cabinet='"++Product++".cab' EmbedCab='yes'\n"
        "          DiskPrompt='CD-ROM #1'/>\n"
        "   <Property Id='DiskPrompt'\n"
        "             Value=\""?COMPANY" "++Product++" Installation [1]\"/>\n\n"),

    ok = file:write(FileH,
        "   <Directory Id='TARGETDIR' Name='SourceDir'>\n"),

    % AppData PATH
    {CoDatId, CoDatGuId} = get_id(Verbose, Tab, component, 'COMPANYDAT_GUID', undefined),
    {AppDatId, AppDatGuId} = get_id(Verbose, Tab, component, 'PRODUCTDAT_GUID', undefined),
    ok = file:write(FileH,
        "     <Directory Id='CommonAppDataFolder' Name='CommonAppData'>\n"
        "       <Directory Id='COMPANYDAT' Name='"?COMPANY"'>\n"
        "         <Component Id='"++CoDatId++"' Guid='"++CoDatGuId++"'>\n"
        "           <CreateFolder Directory='COMPANYDAT'>\n"
        "             <Permission User='Everyone' GenericAll='yes' />\n"
        "           </CreateFolder>\n"
        "         </Component>\n"
        "         <Directory Id='PRODUCTDAT' Name='"++Product++"'>\n"
        "           <Component Id='"++AppDatId++"' Guid='"++AppDatGuId++"'>\n"
        "             <CreateFolder Directory='PRODUCTDAT'>\n"
        "               <Permission User='Everyone' GenericAll='yes' />\n"
        "             </CreateFolder>\n"
        "           </Component>\n"
        "         </Directory> <!-- PRODUCT -->\n"
        "       </Directory> <!-- COMPANY -->\n"
        "     </Directory> <!-- AppDataFolder -->\n\n"),

    % ProgramFiles PATH
    ok = file:write(FileH,
        "     <Directory Id='ProgramFilesFolder' Name='PFiles'>\n"
        "       <Directory Id='"++ID++"' Name='"?COMPANY"'>\n"
        "         <Directory Id='INSTALLDIR' Name='"++Product++"'>\n"),

    walk_release(Verbose, Proj, Tab, FileH, Root),
    
    ok = file:write(FileH,
        "         </Directory> <!-- PRODUCT -->\n"
        "       </Directory> <!-- COMPANY -->\n"
        "     </Directory> <!-- ProgramFilesFolder -->\n"),
    % Property references
    [BootDir] = dets:select(Tab, [{#item{type=dir, name=Version, _='_'}, [],
                                   ['$_']}]),
    [EscriptExe] = dets:select(Tab, [{#item{type=component, name="escript.exe",
                                            _='_'}, [], ['$_']}]),
    [EditConfEs] = dets:select(Tab, [{#item{type=component,
                                             name="editconfs.escript", _='_'},
                                       [], ['$_']}]),
    [Comp] = dets:select(Tab, [{#item{type=component, name=Proj++".cmd", _='_'},
                                [], ['$_']}]),
    [CItm] = dets:select(Tab, [{#item{type=file, name=Proj++".cmd",
                                      guid=undefined, _='_'}, [], ['$_']}]),

    {ProgFolderId, ProgFolderGuId} = get_id(Verbose, Tab, component, 'PROGSMENUFOLDER_GUID', undefined),
    {DsktpShortId, DsktpShortGuId} = get_id(Verbose, Tab, component, 'DESKTOPSHORTCUT_GUID', undefined),

    ok = file:write(FileH,
        "     <Directory Id='ProgramMenuFolder' Name='Programs'>\n"
        "        <Directory Id='ApplicationProgramMenuFolder'\n"
        "                   Name='"++Product++"' />\n"
        "     </Directory> <!-- ProgramMenuFolder -->\n\n"),

    ok = file:write(FileH,
        "     <Directory Id='DesktopFolder' Name='Desktop'>\n"
        "       <Directory Id='ApplicationDesktopFolder' Name='"++Product++"'/>\n"
        "     </Directory> <!-- DesktopFolder -->\n\n"),

    ok = file:write(FileH,
        "   </Directory> <!-- TARGETDIR -->\n\n"),

    build_features(Verbose, Proj, Version, FileH),

    ok = file:write(FileH,
        "   <WixVariable Id='WixUILicenseRtf' Value='License.rtf' />\n"
        "   <WixVariable Id='WixUIBannerBmp' Value='banner493x58.jpg' />\n"
        "   <WixVariable Id='WixUIDialogBmp'"
                       " Value='dialog493x312.jpg' />\n\n"),

    ok = file:write(FileH,
        "   <UIRef Id='WixUI_Mondo' />\n"
        "   <UIRef Id='WixUI_ErrorProgressText' />\n\n"),

    % External Dialog Chaining
    ok = file:write(FileH,
        "   <UI Id='CustWixUI_Mondo'>\n"
        "       <UIRef Id='WixUI_Mondo' />\n"
        "       <UIRef Id='WixUI_ErrorProgressText' />\n\n"

        "       <DialogRef Id='ServiceSetupDlg' />\n"

        "       <Publish Dialog='CustomizeDlg' Control='Next'\n"
        "                Event='NewDialog' Value='ServiceSetupDlg'\n"
        "                Order='3'>LicenseAccepted = 1</Publish>\n"
        "       <Publish Dialog='VerifyReadyDlg' Control='Back'\n"
        "                Event='NewDialog' Value='ServiceSetupDlg'>\n"
        "           1</Publish>\n"
        "   </UI>\n\n"),

    [VmArgsFile] = dets:select(Tab, [{#item{type=file
                                             , name="vm.args", _='_'}
                                       , [], ['$_']}]),
    [SysConfigFile] = dets:select(Tab, [{#item{type=file
                                             , name="sys.config", _='_'}
                                       , [], ['$_']}]),
    {ok, VmArgsBin} = file:read_file(filename:join(VmArgsFile#item.path, "vm.args")),
    {ok, SysConfigBin} = file:read_file(filename:join(SysConfigFile#item.path, "sys.config")),
    {match, [Node]} = re:run(VmArgsBin
                             , ".*-name (.*)[\r\n]"
                             , [{capture, [1], list}, ungreedy, dotall]),
    {match, [Cookie]} = re:run(VmArgsBin
                               , ".*-setcookie (.*)[\r\n]"
                               , [{capture, [1], list}, ungreedy, dotall]),
    {match, [DDErlIntf]} = re:run(SysConfigBin
                                  , ".*\{interface,[ ]*\"(.*)\"[ ]*}"
                                  , [{capture, [1], list}, ungreedy, dotall]),
    {match, [DDErlPort]} = re:run(SysConfigBin
                                  , ".*\{port,[ ]*([0-9]*)[ ]*}"
                                  , [{capture, [1], list}, ungreedy, dotall]),
    {match, [ImemNodeType]} = re:run(SysConfigBin
                                       , ".*\{mnesia_node_type,[ ]*(disc|ram)[ ]*}"
                                       , [{capture, [1], list}, ungreedy, dotall]),
    {match, [ImemSchemaName]} = re:run(SysConfigBin
                                       , ".*\{mnesia_schema_name,[ ]*'(.*)'[ ]*}"
                                       , [{capture, [1], list}, ungreedy, dotall]),
    {match, [ImemClustMgrs]} = re:run(SysConfigBin
                                      , ".*\{erl_cluster_mgrs,[ ]*(\\[.*)[ ]*}"
                                      , [{capture, [1], list}, ungreedy, dotall]),
    {match, [ImemIntf]} = re:run(SysConfigBin
                                 , ".*\{tcp_ip,[ ]*\"(.*)\"[ ]*}"
                                 , [{capture, [1], list}, ungreedy, dotall]),
    {match, [ImemPort]} = re:run(SysConfigBin
                                 , ".*\{tcp_port,[ ]*([0-9]*)[ ]*}"
                                 , [{capture, [1], list}, ungreedy, dotall]),

    ok = file:write(FileH,
        "   <Property Id='NODENAME'>"++Node++"</Property>\n"
        "   <Property Id='NODECOOKIE'>"++Cookie++"</Property>\n"
        "   <Property Id='WEBSRVINTF'>"++DDErlIntf++":"++DDErlPort++"</Property>\n"
        "   <Property Id='DBNODETYPE'>"++ImemNodeType++"</Property>\n"
        "   <Property Id='DBNODETYPE_DISC'>disc</Property>\n"
        "   <Property Id='DBNODETYPE_RAM'>ram</Property>\n"
        "   <Property Id='DBNODESCHEMANAME'>"++ImemSchemaName++"</Property>\n"
        "   <Property Id='DBCLUSTERMGRS'><![CDATA["++ImemClustMgrs++"]]></Property>\n"
        "   <Property Id='DBINTF'>"++ImemIntf++":"++ImemPort++"</Property>\n\n"),

    %% Service customization
    EscriptExePath = filename:split(EscriptExe#item.path),
    EditConfEsPath = filename:split(EditConfEs#item.path),
    ExecCommand = "\"[INSTALLDIR]"
                  ++ string:join(
                       lists:sublist(EscriptExePath
                                     , length(EscriptExePath)-1, 2)
                       ++ ["escript.exe"]
                       , "\\")
                  ++ "\" \"[INSTALLDIR]"
                  ++ string:join(
                       lists:sublist(EditConfEsPath
                                     , length(EditConfEsPath), 1)
                       ++ ["editconfs.escript"]
                       , "\\")
                  ++ "\"",
                  
    if Verbose ->
           io:format("BootDir~n"
                     "     name : ~s~n"
                     "     path : ~s~n"
                     "     id   : ~s~n"
                     , [BootDir#item.name
                        , BootDir#item.path
                        , BootDir#item.id]),
            io:format("ConfigService CMD ~s~n"
                      , [ExecCommand]);
       true ->
            io:format("ConfigService ~s @ ~s~n"
                      , [ExecCommand, BootDir#item.path])
    end,

    %% Service Installation

    % Custom actions service configure
    %  must run after InstallFiles step is 'comitted'
    %  and before InstallService action it must not
    %  impersonate to retain file modification priviledges
    ok = file:write(FileH,
        "   <CustomAction Id='ConfigService' Directory='"++BootDir#item.id++"'\n"
        "                 ExeCommand='"++ExecCommand++" \"[NODENAME]\" "
                                      "\"[NODECOOKIE]\" \"[WEBSRVINTF]\" "
                                      "\"[DBNODETYPE]\" \"[DBNODESCHEMANAME]\" "
                                      "\"[DBCLUSTERMGRS]\" \"[DBINTF]\" "
                                      "\"["++BootDir#item.id++"]\\\" "
                                      "\"[PRODUCTDAT]\\\"'\n"
        "                 Execute='commit' Impersonate='no' />\n\n"),

    % Custom actions service install and start
    %  must run after InstallFiles step is 'comitted'
    % Custom actions service stop and uninstall
    %  must run immediately and before InstallValidate
    %  step to ensure that installed files are not
    %  removed and DDErl service is stopped before
    %  uninstalling process detecets and warns
    ok = file:write(FileH,
        "   <CustomAction Id='InstallService'\n"
        "                 FileKey='"++CItm#item.id++"'\n"
        "                 ExeCommand='install' Execute='commit' Impersonate='no' />\n"
        "   <CustomAction Id='StartService' FileKey='"++CItm#item.id++"'\n"
        "                 ExeCommand='start' Execute='commit' Impersonate='no' />\n"
        "   <CustomAction Id='UnInstallService'\n"
        "                 FileKey='"++CItm#item.id++"'\n"
        "                 ExeCommand='uninstall' Execute='deferred' Impersonate='no' />\n"
        "   <CustomAction Id='StopService' FileKey='"++CItm#item.id++"'\n"
        "                 ExeCommand='stop' Execute='deferred' Impersonate='no' />\n\n"),

    % Sequence of custom action is important to ensure
    %  service is installed before started and stopped
    %  before uninstalling
    % Also ComponentId = # is used to identify the service
    %  controlling script is executed in correct execution path
    %  2 - Uninstalling
    %  3 - Installing
    %  Ref http://wix.tramontana.co.hu/tutorial/com-expression-syntax-miscellanea/expression-syntax
    ok = file:write(FileH,
        "   <InstallExecuteSequence>\n"
        "      <Custom Action='StopService' After='InstallInitialize'>"
                "$"++Comp#item.id++"=2</Custom>\n"
        "      <Custom Action='UnInstallService' After='StopService'>"
                "$"++Comp#item.id++"=2</Custom>\n"
        "      <Custom Action='ConfigService' After='InstallFiles'>"
                "$"++EscriptExe#item.id++"=3 AND "
                "$"++EditConfEs#item.id++"=3</Custom>\n"
        "      <Custom Action='InstallService' After='ConfigService'>"
                "$"++Comp#item.id++"=3</Custom>\n"
        "      <Custom Action='StartService' After='InstallService'>"
                "$"++Comp#item.id++"=3</Custom>\n"
        "   </InstallExecuteSequence>\n\n"),

    ok = file:write(FileH,
        "   <DirectoryRef Id='ApplicationProgramMenuFolder'>\n"
        "       <Component Id='"++ProgFolderId++"' Guid='"++ProgFolderGuId++"'>\n"
        "           <Shortcut Id='programattach'\n"
        "                     Name='"++Proj++" Attach'\n"
        "                     Target='[#"++CItm#item.id++"]'\n"
        "                     Arguments='attach'\n"
        "                     WorkingDirectory='"++BootDir#item.id++"'\n"
        "                     Icon='"++Proj++".ico' IconIndex='0' />\n"
        "           <Shortcut Id='programgui'\n"
        "                     Name='"++Proj++" GUI'\n"
        "                     Target='[#"++CItm#item.id++"]'\n"
        "                     Arguments='console'\n"
        "                     WorkingDirectory='"++BootDir#item.id++"'\n"
        "                     Icon='"++Proj++".ico' IconIndex='0' />\n"
        "           <RemoveFolder Id='ApplicationProgramMenuFolder' On='uninstall' />\n"
        "           <RegistryValue Root='HKCU'\n"
        "                          Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                          Name='programmenu' Type='string'\n"
        "                          Value='"++PRODUCT_GUID++"' KeyPath='yes'/>\n"
        "       </Component>\n"
        "   </DirectoryRef>\n"),

    ok = file:write(FileH,
        "   <DirectoryRef Id='ApplicationDesktopFolder'>\n"
        "       <Component Id='"++DsktpShortId++"' Guid='"++DsktpShortGuId++"'>\n"
        "           <Shortcut Id='desktopattach'\n"
        "                     Name='"++Proj++" Attach'\n"
        "                     Target='[#"++CItm#item.id++"]'\n"
        "                     Arguments='attach'\n"
        "                     WorkingDirectory='"++BootDir#item.id++"'\n"
        "                     Icon='"++Proj++".ico' IconIndex='0' />\n"
        "           <Shortcut Id='desktopgui'\n"
        "                     Name='"++Proj++" GUI'\n"
        "                     Target='[#"++CItm#item.id++"]'\n"
        "                     Arguments='console'\n"
        "                     WorkingDirectory='"++BootDir#item.id++"'\n"
        "                     Icon='"++Proj++".ico' IconIndex='0' />\n"
        "           <RemoveFolder Id='ApplicationDesktopFolder' On='uninstall'/>\n"
        "           <RegistryValue Root='HKCU'\n"
        "                          Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                          Name='desktop' Type='integer'\n"
        "                          Value='1' KeyPath='yes'/>\n"
        "       </Component>\n"
        "   </DirectoryRef>\n"),

    ok = file:write(FileH,
        "   <Icon Id='"++Proj++".ico' SourceFile='"++Proj++".ico' />\n"),

    ok = file:write(FileH,
        "   <Property Id='ARPPRODUCTICON' Value='"++Proj++".ico' />"),
    
    ok = file:write(FileH,
        "</Product>\n"
        "</Wix>"),

    ok = file:close(FileH).

candle_light(Verbose, Proj, Version, Root) ->
    {ok, CurDir} = file:get_cwd(),
    ok = file:set_cwd(filename:join([Root,"rel","wixsetup"])),
    Wxses = filelib:wildcard("*.wxs"),
    run_port(os:find_executable("candle.exe"),
             if Verbose -> ["-v"]; true -> [] end ++ Wxses),
    WixObjs = filelib:wildcard("*.wixobj"),
    MsiFile = generate_msi_name(Proj,Version),
    run_port(os:find_executable("light.exe"),
             if Verbose -> ["-v"]; true -> [] end
             ++ ["-ext","WixUIExtension","-out",MsiFile | WixObjs]),
    ok = file:set_cwd(CurDir).

get_filepath(Dir, F) ->
    FilePathNoRel =
        lists:foldl(
          fun
              ("..", Acc) -> Acc;
              (P,Acc) -> Acc ++ [P]
          end,
          [], filename:split(Dir)),
    filename:join([".." | FilePathNoRel]++[F]).

generate_msi_name(Proj,Version) ->
    {{Y,M,D},{H,Mn,S}} = calendar:local_time(),
    MsiDate = io_lib:format("~4..0B~2..0B~2..0B_~2..0B~2..0B~2..0B",
                            [Y,M,D,H,Mn,S]),
    lists:flatten([Proj,"-",Version,"_",MsiDate,".msi"]).

walk_release(Verbose, Proj, Tab, FileH, Root) ->
    ReleaseRoot = filename:join([Root,"rel",Proj]),
    case filelib:is_dir(ReleaseRoot) of
        true ->
            walk_release(Verbose, Tab, FileH,
                         filelib:wildcard("*", ReleaseRoot), ReleaseRoot, 12);
        false -> ?L("~p is not a directory", [ReleaseRoot])
    end.

walk_release(_Verbose, _Tab, _FileH, [], _Dir, _N) -> ok;
walk_release(Verbose, Tab, FileH, [F|Files], Dir, N) ->
    case filelib:is_dir(filename:join([Dir,F])) of
        true ->
            NewDirLevel = filename:join([Dir,F]),
            FilesAtThisLevel = filelib:wildcard("*", NewDirLevel),
            {ok, DirId} = get_id(Verbose, Tab, dir, F, Dir),
            ok = file:write(FileH, lists:duplicate(N,32)++
                            "<Directory Id='"++DirId++
                                            "' Name='"++F++"'>\n"),
            walk_release(Verbose, Tab, FileH, FilesAtThisLevel, NewDirLevel,
                         N+3),
            ok = file:write(FileH, lists:duplicate(N,32)++"</Directory>\n"),
            ?L("~s/", [NewDirLevel]);
        false ->
            FilePath = get_filepath(Dir, F),
            {Id, GuID} = get_id(Verbose, Tab, component, F, Dir),
            {ok, FileId} = get_id(Verbose, Tab, file, F, Dir),
            ok = file:write(FileH, lists:duplicate(N+3,32)++
                "<Component Id='"++Id++"' Guid='"++GuID++"'>\n"
                ++lists:duplicate(N+3,32)++
                "   <File Id='"++FileId++"' Name='"++F++
                                "' DiskId='1' Source='"++FilePath++"'"
                " KeyPath='yes' />\n"++lists:duplicate(N+3,32)++
                "</Component>\n")
    end,
    walk_release(Verbose, Tab, FileH, Files, Dir, N).

build_features(_Verbose, Proj, Version, FileH) ->
    Tab = Proj++"ids",
    ok = file:write(FileH,
        "   <Feature Id='Complete' Title='"++Proj++"-"++Version++"'"
                    " Description='The complete package.'"
                    " Level='1' ConfigurableDirectory='INSTALLDIR'>\n"),
    ok = file:write(FileH,
        "      <Feature Id='MainProgram' Title='"++Proj++"-"++Version
                                                        ++" service'"
                    " Description='The service.' Level='1'>\n"),
    dets:sync(Tab),
    dets:traverse(Tab,
                  fun(#item{type=component, id = Id}) ->
                          ok = file:write(FileH
                                          , "         <ComponentRef Id='"
                                          ++Id++"' />\n"),
                          continue;
                     (_) -> continue
                  end),
    ok = file:write(FileH, "      </Feature>\n\n"),
    ok = file:write(FileH, "   </Feature>\n\n").

get_id(_Verbose, Tab, undefined, Field, undefined) when is_list(Field) ->
    Id = "id_"++?H(Field),
    case dets:lookup(Tab, Id) of
        [] ->
            Item = #item{id = Id, name = Field},
            ok = dets:insert(Tab, Item),
            {ok, Item#item.id};
        [#item{} = Item] ->
            {ok, Item#item.id}
    end;
get_id(_Verbose, Tab, Type, Field, undefined) when is_atom(Field) ->
    Id = "id_"++?H(Field),
    case dets:lookup(Tab, Id) of
        [] ->
            Item = #item{id = Id, name = Field, guid = uuid()},
            case Type of
                component ->
                    ok = dets:insert(Tab, Item#item{type=component}),
                    {Item#item.id, Item#item.guid};
                _ ->
                    ok = dets:insert(Tab, Item),
                    {ok, Item#item.guid}
            end;
        [#item{} = Item] ->
            case Type of
                component -> {Item#item.id, Item#item.guid};
                _ -> {ok, Item#item.guid}
            end
    end;
get_id(_Verbose, Tab, Type, F, Dir)
  when Type =:= component;
       Type =:= file;
       Type =:= dir ->
    Id = "id_"++?H({Type, filename:join([Dir, F])}),
    {ok, FI} = file:read_file_info(filename:join([Dir, F])),
    case dets:lookup(Tab, Id) of
        [] ->
            Item = #item{id = Id
                         , type = Type
                         , guid = if Type =:= component -> uuid();
                                     true -> undefined end
                         , name = F
                         , path = Dir
                         , file_info = FI
                        },
            ok = dets:insert(Tab, Item),
            case Type of
                component -> {Item#item.id, Item#item.guid};
                file -> {ok, Item#item.id};
                dir -> {ok, Item#item.id}
            end;
        [#item{name = F, path = Dir, file_info = FI} = Item] ->
            case Type of
                component -> {Item#item.id, Item#item.guid};
                file -> {ok, Item#item.id};
                dir -> {ok, Item#item.id}
            end;
        [#item{name = F, path = Dir} = I] ->
            Item = I#item{guid = if Type =:= component -> uuid();
                                    true -> undefined end
                          , file_info = FI},
            ok = dets:insert(Tab, Item),
            case Type of
                component -> {Item#item.id, Item#item.guid};
                file -> {ok, Item#item.id};
                dir -> {ok, Item#item.id}
            end
    end.
