#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -mnesia debug verbose
-include_lib("kernel/include/file.hrl").

-define(COMPANY, "K2 Informatics GmbH").
-define(PRODUCT, "DDErl").
-define(APP_NAME, "dderl").
-define(VERSION, "1.0.7").
-define(PKG_COMMENT, "DDErl is a registered trademark of"
                     " K2 Informatics GmbH").
-define(WXSFILE, "dderl.wxs").
-define(TAB, "dderlids").
-define(TABFILE, "dderlids.dets").

-define(H(__F), integer_to_list(erlang:phash2(__F), 16)).

-define(TRACE,  io:format("TRACE ~p~n", [?LINE])).

-record(item, { id
              , type % file | dir | component
              , guid
              , name
              , path
              , file_info
        }).

main([]) ->
    io:format("Building MSI from master tag silently~n"),
    main({false, "master"});
main(["-v"]) ->
    io:format("Building MSI from master tag with progress logs~n"),
    main({true, "master"});
main(["-t"]) ->
    io:format("Available tags~n"),
    Tags = lists:sort(string:tokens(oscmd(false, "git tag"), "\n")),
    [io:format("   ~s~n", [T]) || T <- Tags];
main(["-v", Tag]) ->
    io:format("Building MSI from ~s tag with progress logs~n", [Tag]),
    main({true, Tag});
main([Tag]) ->
    io:format("Building MSI from ~s tag silently~n", [Tag]),
    main({false, Tag});
main({Verbose, _Tag}) -> % Target Specified
    %prepare_release(Verbose, Tag);
    {Root, AppPath} = get_paths(),
    build_msi(Verbose, Root, AppPath);
main(_) ->
    usage().

oscmd(Verbose, Cmd) ->
    CmdRes = re:replace(os:cmd(Cmd), "[\r\n]", "", [{return, list}]),
    if Verbose -> io:format("~s -> ~s~n", [Cmd, CmdRes]); true -> ok end,
    CmdRes.

%prepare_release(Verbose, _Tag) ->
%    Pwd = filename:dirname(escript:script_name()),
%    GitRepo = oscmd(Verbose,"git config --get remote.origin.url"),
%    DistroFolder = string:join([Pwd, "distro"], "\\"),
%    io:format("Pwd ~s~n", [Pwd]),
%    io:format("GitRepo ~s~n", [GitRepo]),
%    {ok, CurDir} = file:get_cwd(),
%    %io:format("Removing ~s~n", [DistroFolder]),
%    %oscmd(Verbose, "rd "++DistroFolder++" /s /q"),
%    %io:format("Creating empty ~s~n", [DistroFolder]),
%    %ok = file:make_dir(DistroFolder),
%    %ok = file:set_cwd(DistroFolder),
%    %oscmd(Verbose,"git clone "++GitRepo),
%    AppRootPath = string:join([DistroFolder, "dderl"], "\\"),
%    AppRootFolder = filename:absname(AppRootPath),
%    ok = file:set_cwd(AppRootPath),
%    io:format("downloading dependencies...~n"),
%    oscmd(Verbose, "rebar get-deps"),
%    AppReleaseFolder = filename:absname(string:join([AppRootFolder, "apps\\dderl"], "\\")),
%    io:format("moving src/ priv/ from ~s to ~s~n", [AppRootFolder, AppReleaseFolder]),
%    oscmd(Verbose, "move \""
%                   ++ string:join([AppRootFolder, "src"], "\\")
%                   ++ "\" \""
%                   ++ AppReleaseFolder
%                   ++ "\""),
%    oscmd(Verbose, "move \""
%                   ++ string:join([AppRootFolder, "priv"], "\\")
%                   ++ "\" \""
%                   ++ AppReleaseFolder
%                   ++ "\""),
%    ok = file:set_cwd(CurDir).

build_msi(Verbose, Root, AppPath) ->
    io:format("Root ~s~n", [Root]),
    io:format("AppPath ~s~n", [AppPath]),
    make_soft_links(Verbose, AppPath),
    rebar_generate(Verbose, Root),
    {ok, ?TAB} = dets:open_file(?TAB, [{ram_file, true}
                                       , {file, ?TABFILE}
                                       , {keypos, 2}]),
    create_wxs(Verbose, Root),
    ok = dets:close(?TAB).
    
usage() ->
    io:format("usage: build_msi.escript [[-v] Tag | -t] ~n"),
    io:format("       default Tag 'master'~n"),
    halt(1).

uuid() ->
    string:to_upper(re:replace(os:cmd("uuidgen.exe")
              , "\r\n", "", [{return, list}])).

get_paths() ->
    case lists:reverse(filename:split(
                        filename:dirname(escript:script_name()))) of
        ["."] -> {"../../", "../../apps/dderl"};
        [_,_|SomeOtherPath] ->
            R = lists:reverse(SomeOtherPath),
            {filename:join(R), filename:join(R++["apps","dderl"])}
    end.

make_soft_links(Verbose, AppPath) ->
    [begin
        Cmd = lists:flatten(io_lib:format("mklink /D ~p ~p"
                                          , [Link, Target])),
        io:format("Creating soft links ~s <- ~s~n", [Target, Link]),
        if Verbose -> io:format("Executing ~s~n", [Cmd]); true -> ok end,
        io:format("    ~s~n", [os:cmd(Cmd)])
     end
    || {Link, Target} <- [ {filename:join([AppPath, "src"]), "..\\..\\src"}
                          , {filename:join([AppPath, "priv"])
                             , "..\\..\\priv"}]].

rebar_generate(Verbose, Root) ->
    file:delete(?TABFILE),
    {ok, CurDir} = file:get_cwd(),
    if Verbose ->
           io:format("Entering ~s from ~s~n", [Root, CurDir]);
       true -> ok
    end,
    ok = file:set_cwd(Root),
    io:format("Clean Compile and generate...~n", []),
    io:format("~s", [os:cmd("rebar "++if Verbose -> "-vvv"; true -> "" end++" clean")]),
    io:format("~s", [os:cmd("rebar "++if Verbose -> "-vvv"; true -> "" end++" compile")]),
    io:format("~s", [os:cmd("rebar "++if Verbose -> "-vvv"; true -> "" end++" generate skip_deps=true")]),
    if Verbose ->
           io:format("Leaving ~s to ~s~n", [Root, CurDir]);
       true -> ok
    end,
    ok = file:set_cwd(CurDir).

create_wxs(Verbose, Root) ->
    {ok, FileH} = file:open(filename:join([Root, "rel"
                                           , "wixsetup", ?WXSFILE])
                            , [write, raw]),
    {ok, PRODUCT_GUID} = get_id(Verbose, undefined, 'PRODUCT_GUID', undefined),
    {ok, UPGRADE_GUID} = get_id(Verbose, undefined, 'UPGRADE_GUID', undefined),
    {ok, ID} = get_id(Verbose, undefined, ?COMPANY, undefined),
    ok = file:write(FileH,
        "<?xml version='1.0' encoding='windows-1252'?>\n"
        "<Wix xmlns='http://schemas.microsoft.com/wix/2006/wi'>\n\n"

        "<Product Name='"?PRODUCT" "?VERSION"'\n"
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

        "   <Media Id='1' Cabinet='"?PRODUCT".cab' EmbedCab='yes'\n"
        "          DiskPrompt='CD-ROM #1'/>\n"
        "   <Property Id='DiskPrompt'\n"
        "             Value=\""?COMPANY
                            " "?VERSION" Installation [1]\"/>\n\n"),

    ok = file:write(FileH,
        "   <Directory Id='TARGETDIR' Name='SourceDir'>\n"),

    % AppData PATH
    {CoDatId, CoDatGuId} = get_id(Verbose, component, 'COMPANYDAT_GUID', undefined),
    {AppDatId, AppDatGuId} = get_id(Verbose, component, 'PRODUCTDAT_GUID', undefined),
    ok = file:write(FileH,
        "     <Directory Id='CommonAppDataFolder' Name='CommonAppData'>\n"
        "       <Directory Id='COMPANYDAT' Name='"?COMPANY"'>\n"
        "         <Component Id='"++CoDatId++"' Guid='"++CoDatGuId++"'>\n"
        "           <CreateFolder Directory='COMPANYDAT'>\n"
        "             <Permission User='Everyone' GenericAll='yes' />\n"
        "           </CreateFolder>\n"
        "         </Component>\n"
        "         <Directory Id='PRODUCTDAT' Name='"?PRODUCT" "?VERSION"'>\n"
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
        "         <Directory Id='INSTALLDIR' Name='"?PRODUCT
                                                   " "?VERSION"'>\n"),

    walk_release(Verbose, FileH, Root),
    
    ok = file:write(FileH,
        "         </Directory> <!-- PRODUCT -->\n"
        "       </Directory> <!-- COMPANY -->\n"
        "     </Directory> <!-- ProgramFilesFolder -->\n"),

    % Property references
    [BootDir] = dets:select(?TAB, [{#item{type=dir
                                          , name="1.0.7", _='_'}
                                    , [], ['$_']}]),
    [EscriptExe] = dets:select(?TAB, [{#item{type=component
                                              , name="escript.exe", _='_'}
                                        , [], ['$_']}]),
    [EditConfEs] = dets:select(?TAB, [{#item{type=component
                                             , name="editconfs.escript", _='_'}
                                       , [], ['$_']}]),
    [Comp] = dets:select(?TAB, [{#item{type=component
                                       , name="dderl.cmd", _='_'}
                                 , [], ['$_']}]),
    [CItm] = dets:select(?TAB, [{#item{type=file, name="dderl.cmd"
                                       , guid=undefined, _='_'}
                                 , [], ['$_']}]),

    {ProgFolderId, ProgFolderGuId} = get_id(Verbose, component, 'PROGSMENUFOLDER_GUID', undefined),
    {DsktpShortId, DsktpShortGuId} = get_id(Verbose, component, 'DESKTOPSHORTCUT_GUID', undefined),

    ok = file:write(FileH,
        "     <Directory Id='ProgramMenuFolder' Name='Programs'>\n"
        "        <Directory Id='ApplicationProgramMenuFolder'\n"
        "                   Name='"?PRODUCT" "?VERSION"' />\n"
        "     </Directory> <!-- ProgramMenuFolder -->\n\n"),

    ok = file:write(FileH,
        "     <Directory Id='DesktopFolder' Name='Desktop'>\n"
        "       <Directory Id='ApplicationDesktopFolder' Name='"?PRODUCT" "?VERSION"'/>\n"
        "     </Directory> <!-- DesktopFolder -->\n\n"),

    ok = file:write(FileH,
        "   </Directory> <!-- TARGETDIR -->\n\n"),

    build_features(Verbose, FileH),

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

    [VmArgsFile] = dets:select(?TAB, [{#item{type=file
                                             , name="vm.args", _='_'}
                                       , [], ['$_']}]),
    [SysConfigFile] = dets:select(?TAB, [{#item{type=file
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
        "                     Name='"?PRODUCT" Attach'\n"
        "                     Target='[#"++CItm#item.id++"]'\n"
        "                     Arguments='attach'\n"
        "                     WorkingDirectory='"++BootDir#item.id++"'\n"
        "                     Icon='dderl.ico' IconIndex='0' />\n"
        "           <Shortcut Id='programgui'\n"
        "                     Name='"?PRODUCT" GUI'\n"
        "                     Target='[#"++CItm#item.id++"]'\n"
        "                     Arguments='console'\n"
        "                     WorkingDirectory='"++BootDir#item.id++"'\n"
        "                     Icon='dderl.ico' IconIndex='0' />\n"
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
        "                     Name='"?PRODUCT" Attach'\n"
        "                     Target='[#"++CItm#item.id++"]'\n"
        "                     Arguments='attach'\n"
        "                     WorkingDirectory='"++BootDir#item.id++"'\n"
        "                     Icon='dderl.ico' IconIndex='0' />\n"
        "           <Shortcut Id='desktopgui'\n"
        "                     Name='"?PRODUCT" GUI'\n"
        "                     Target='[#"++CItm#item.id++"]'\n"
        "                     Arguments='console'\n"
        "                     WorkingDirectory='"++BootDir#item.id++"'\n"
        "                     Icon='dderl.ico' IconIndex='0' />\n"
        "           <RemoveFolder Id='ApplicationDesktopFolder' On='uninstall'/>\n"
        "           <RegistryValue Root='HKCU'\n"
        "                          Key='Software\\[Manufacturer]\\[ProductName]'\n"
        "                          Name='desktop' Type='integer'\n"
        "                          Value='1' KeyPath='yes'/>\n"
        "       </Component>\n"
        "   </DirectoryRef>\n"),

    ok = file:write(FileH,
        "   <Icon Id='dderl.ico' SourceFile='dderl.ico' />\n"),

    ok = file:write(FileH,
        "   <Property Id='ARPPRODUCTICON' Value='dderl.ico' />"),
    
    ok = file:write(FileH,
        "</Product>\n"
        "</Wix>"),

    ok = file:close(FileH),
    {ok, CurDir} = file:get_cwd(),
    ok = file:set_cwd(filename:join([Root,"rel", "wixsetup"])),
    Wxses = filelib:wildcard("*.wxs"),
    CandleCmd = "candle.exe "
                ++ if Verbose -> "-v "; true -> "" end
                ++ string:join(Wxses, " "),
    io:format("~s~n", [CandleCmd]),
    io:format("~s", [os:cmd(CandleCmd)]),
    WixObjs = filelib:wildcard("*.wixobj"),
    MsiFile = generate_msi_name(),
    LightCmd = "light.exe "
                ++ if Verbose -> "-v "; true -> "" end
                ++ "-ext WixUIExtension -out "
                ++ MsiFile
                ++ " "
                ++ string:join(WixObjs, " "),
    io:format("~s~n", [LightCmd]),
    io:format("~s", [os:cmd(LightCmd)]),
    ok = file:set_cwd(CurDir).

get_filepath(Dir, F) ->
    FilePathNoRel =
        lists:foldl(
          fun
              ("..", Acc) -> Acc;
              ("rel", Acc) -> Acc;
              (P,Acc) -> Acc ++ [P]
          end,
          [], filename:split(Dir)),
    filename:join([".." | FilePathNoRel]++[F]).

generate_msi_name() ->
    MsiDate = format_date_msi(calendar:local_time()),
    ?APP_NAME ++ "_" ++ MsiDate ++ ".msi".

format_date_msi({{Y, M, D},{H, Min, S}}) ->
    AsString = [to_list_add_padding(X) || X <- [Y,M,D,H,Min,S]],
    string:join(AsString, "-").

to_list_add_padding(Value) when Value < 10 ->
    [$0 | integer_to_list(Value)];
to_list_add_padding(Value) ->
    integer_to_list(Value).

walk_release(Verbose, FileH, Root) ->
    ReleaseRoot = filename:join([Root,"rel","dderl"]),
    case filelib:is_dir(ReleaseRoot) of
        true ->
            walk_release(Verbose, FileH, filelib:wildcard("*", ReleaseRoot)
                         , ReleaseRoot, 12);
        false -> io:format("~p is not a directory~n", [ReleaseRoot])
    end.

walk_release(_Verbose, _FileH, [], _Dir, _N) -> ok;
walk_release(Verbose, FileH, [F|Files], Dir, N) ->
    case filelib:is_dir(filename:join([Dir,F])) of
        true ->
            NewDirLevel = filename:join([Dir,F]),
            FilesAtThisLevel = filelib:wildcard("*", NewDirLevel),
            {ok, DirId} = get_id(Verbose, dir, F, Dir),
            ok = file:write(FileH, lists:duplicate(N,32)++
                            "<Directory Id='"++DirId++
                                            "' Name='"++F++"'>\n"),
            walk_release(Verbose, FileH, FilesAtThisLevel, NewDirLevel, N+3),
            ok = file:write(FileH, lists:duplicate(N,32)++"</Directory>\n"),
            if Verbose -> io:format("~s/ ->~n", [NewDirLevel]);
               true -> io:format(lists:duplicate(N-10,32)++"~s/~n", [F])
            end;
        false ->
            FilePath = get_filepath(Dir, F),
            {Id, GuID} = get_id(Verbose, component, F, Dir),
            {ok, FileId} = get_id(Verbose, file, F, Dir),
            ok = file:write(FileH, lists:duplicate(N+3,32)++
                "<Component Id='"++Id++"' Guid='"++GuID++"'>\n"
                ++lists:duplicate(N+3,32)++
                "   <File Id='"++FileId++"' Name='"++F++
                                "' DiskId='1' Source='"++FilePath++"'"
                " KeyPath='yes' />\n"++lists:duplicate(N+3,32)++
                "</Component>\n"),
            if Verbose -> io:format("\t~s~n", [F]); true -> ok end
    end,
    walk_release(Verbose, FileH, Files, Dir, N).

build_features(_Verbose, FileH) ->
    ok = file:write(FileH,
        "   <Feature Id='Complete' Title='"?PRODUCT" "?VERSION"'"
                    " Description='The complete package.'"
                    " Level='1' ConfigurableDirectory='INSTALLDIR'>\n"),
    ok = file:write(FileH,
        "      <Feature Id='MainProgram' Title='"?PRODUCT" "
                                                ?VERSION" service'"
                    " Description='The service.' Level='1'>\n"),
    dets:sync(?TAB),
    dets:traverse(?TAB,
                  fun(#item{type=component, id = Id}) ->
                          ok = file:write(FileH
                                          , "         <ComponentRef Id='"
                                          ++Id++"' />\n"),
                          continue;
                     (_) -> continue
                  end),
    ok = file:write(FileH, "      </Feature>\n\n"),
    ok = file:write(FileH, "   </Feature>\n\n").

get_id(_Verbose, undefined, Field, undefined) when is_list(Field) ->
    Id = "id_"++?H(Field),
    case dets:lookup(?TAB, Id) of
        [] ->
            Item = #item{id = Id, name = Field},
            ok = dets:insert(?TAB, Item),
            {ok, Item#item.id};
        [#item{} = Item] ->
            {ok, Item#item.id}
    end;
get_id(_Verbose, Type, Field, undefined) when is_atom(Field) ->
    Id = "id_"++?H(Field),
    case dets:lookup(?TAB, Id) of
        [] ->
            Item = #item{id = Id, name = Field, guid = uuid()},
            case Type of
                component ->
                    ok = dets:insert(?TAB, Item#item{type=component}),
                    {Item#item.id, Item#item.guid};
                _ ->
                    ok = dets:insert(?TAB, Item),
                    {ok, Item#item.guid}
            end;
        [#item{} = Item] ->
            case Type of
                component -> {Item#item.id, Item#item.guid};
                _ -> {ok, Item#item.guid}
            end
    end;
get_id(_Verbose, Type, F, Dir)
  when Type =:= component;
       Type =:= file;
       Type =:= dir ->
    Id = "id_"++?H({Type, filename:join([Dir, F])}),
    {ok, FI} = file:read_file_info(filename:join([Dir, F])),
    case dets:lookup(?TAB, Id) of
        [] ->
            Item = #item{id = Id
                         , type = Type
                         , guid = if Type =:= component -> uuid();
                                     true -> undefined end
                         , name = F
                         , path = Dir
                         , file_info = FI
                        },
            ok = dets:insert(?TAB, Item),
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
            ok = dets:insert(?TAB, Item),
            case Type of
                component -> {Item#item.id, Item#item.guid};
                file -> {ok, Item#item.id};
                dir -> {ok, Item#item.id}
            end
    end.
