#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname build_msi -mnesia debug verbose
-include_lib("kernel/include/file.hrl").

-define(COMPANY, "K2 Informatics GmbH").
-define(PRODUCT, "DDErl").
-define(VERSION, "1.0.7").
-define(PKG_COMMENT, "DDErl is a registered trademark of"
                     " K2 Informatics GmbH").
-define(WXSFILE, "dderl.wxs").
-define(MSIFILE, "dderl.msi").
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

main([]) -> main(["not_verbose"]);
main([Vrbs]) ->
    {Root, AppPath} = get_paths(),
    io:format("Root ~s~n", [Root]),
    io:format("AppPath ~s~n", [AppPath]),
    Verbose = case Vrbs of
                  "-v" ->
                      io:format("Verbose true~n"),
                      true;
                  "not_verbose" -> false
              end,
    %make_soft_links(Verbose, AppPath),
    %rebar_generate(Verbose, Root),
    {ok, ?TAB} = dets:open_file(?TAB, [{ram_file, true}
                                       , {file, ?TABFILE}
                                       , {keypos, 2}]),
    create_wxs(Verbose, Root),
    ok = dets:close(?TAB);
main(_) ->
    usage().

usage() ->
    io:format("usage: build_msi.escript [-v]~n"),
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

make_soft_links(AppPath) ->
    [begin
        Cmd = lists:flatten(io_lib:format("mklink /D ~p ~p"
                                          , [Link, Target])),
        io:format("~s~n~s~n", [Cmd, os:cmd(Cmd)])
     end
    || {Link, Target} <- [ {filename:join([AppPath, "src"]), "..\\..\\src"}
                          , {filename:join([AppPath, "priv"])
                             , "..\\..\\priv"}]].

rebar_generate(Root) ->
    {ok, CurDir} = file:get_cwd(),
    ok = file:set_cwd(Root),
    io:format("Clean Compile and generate...~n", []),
    io:format("~s", [os:cmd("rebar clean")]),
    io:format("~s", [os:cmd("rebar compile")]),
    io:format("~s", [os:cmd("rebar generate")]),
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
        "            InstallerVersion='100' Languages='1033'\n"
        "            Compressed='yes'\n"
        "            SummaryCodepage='1252' />\n\n"

        "   <Media Id='1' Cabinet='"?PRODUCT".cab' EmbedCab='yes'\n"
        "          DiskPrompt='CD-ROM #1'/>\n"
        "   <Property Id='DiskPrompt'\n"
        "             Value=\""?COMPANY
                            " "?VERSION" Installation [1]\"/>\n\n"

        "   <Directory Id='TARGETDIR' Name='SourceDir'>\n"
        "     <Directory Id='ProgramFilesFolder' Name='PFiles'>\n"
        "       <Directory Id='"++ID++"' Name='"?COMPANY"'>\n"
        "         <Directory Id='INSTALLDIR' Name='"?PRODUCT
                                            " "?VERSION"'>\n"),

    walk_release(Verbose, FileH, Root),

    ok = file:write(FileH,
        "         </Directory>\n"
        "       </Directory>\n"
        "     </Directory>\n"),

    {RegId, RegGuId} = get_id(Verbose, component, "dderl"
                              , filename:join([Root,"rel"])),
    ok = file:write(FileH,
        "     <Directory Id='ProgramMenuFolder' Name='Programs'>\n"
        "        <Directory Id='"++RegId++"'"
                                " Name='"?PRODUCT" "?VERSION"'>\n"
        "           <Component Id='"++RegId++"' Guid='"++RegGuId++"'>\n"
        "               <RemoveFolder Id='"++RegId++"' On='uninstall' />\n"
        "               <RegistryValue Root='HKCU'"
                            " Key='Software\\[Manufacturer]\\[ProductName]'"
                            " Type='string' Value='' KeyPath='yes' />\n"
        "           </Component>\n"
        "       </Directory>\n"
        "     </Directory>\n\n"),

    ok = file:write(FileH,
        "     <Directory Id='DesktopFolder' Name='Desktop' />\n"
        "   </Directory>\n\n"),

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

    ok = file:write(FileH,
        "   <Property Id='NODENAME'>dderl@127.0.0.1</Property>\n"
        "   <Property Id='NODECOOKIE'>dderlcookie</Property>\n"
        "   <Property Id='WEBSRVINTF'>127.0.0.1:8443</Property>\n"
        "   <Property Id='DBNODETYPE'>disc</Property>\n"
        "   <Property Id='DBNODESCHEMANAME'>dderlstag</Property>\n"
        "   <Property Id='DBCLUSTERMGRS'><![CDATA[['dderl@127.0.0.1']]]></Property>\n"
        "   <Property Id='DBINTF'>127.0.0.1:1234</Property>\n\n"),

    %% Service Installation
    [Comp] = dets:select(?TAB, [{#item{type=component
                                       , name="dderl.cmd", _='_'}
                                 , [], ['$_']}]),
    [CItm] = dets:select(?TAB, [{#item{type=file, name="dderl.cmd"
                                       , guid=undefined, _='_'}
                                 , [], ['$_']}]),

    % Custom actions service install and start
    %  must run after InstallFiles step is 'comitted'
    % Custom actions service stop and uninstall
    %  must run immediately and before InstallValidate
    %  step to ensure that installed files are not
    %  removed and DDErl service is stopped before
    %  uninstalling process detecets and warns
    ok = file:write(FileH,
        "   <CustomAction Id='InstallService' FileKey='"++CItm#item.id++"'\n"
        "                 ExeCommand='install' Execute='commit' />\n"
        "   <CustomAction Id='StartService' FileKey='"++CItm#item.id++"'\n"
        "                 ExeCommand='start' Execute='commit' />\n"
        "   <CustomAction Id='UnInstallService' FileKey='"++CItm#item.id++"'\n"
        "                 ExeCommand='uninstall' Execute='immediate' />\n"
        "   <CustomAction Id='StopService' FileKey='"++CItm#item.id++"'\n"
        "                 ExeCommand='stop' Execute='immediate' />\n\n"
                   ),

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
        "      <Custom Action='StopService' Before='InstallValidate'>"
                "$"++Comp#item.id++"=2</Custom>\n"
        "      <Custom Action='UnInstallService' Before='InstallValidate'>"
                "$"++Comp#item.id++"=2</Custom>\n"
        "      <Custom Action='InstallService' After='InstallFiles'>"
                "$"++Comp#item.id++"=3</Custom>\n"
        "      <Custom Action='StartService' After='InstallFiles'>"
                "$"++Comp#item.id++"=3</Custom>\n"
        "   </InstallExecuteSequence>\n\n"),

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
    LightCmd = "light.exe "
                ++ if Verbose -> "-v "; true -> "" end
                ++ "-ext WixUIExtension -out "?MSIFILE" "
                ++ string:join(WixObjs, " "),
    io:format("~s~n", [LightCmd]),
    io:format("~s", [os:cmd(LightCmd)]),
    ok = file:set_cwd(CurDir).

walk_release(Verbose, FileH, Root) ->
    ReleaseRoot = filename:join([Root,"rel","dderl"]),
    case filelib:is_dir(ReleaseRoot) of
        true ->
            walk_release(Verbose, FileH, filelib:wildcard("*", ReleaseRoot)
                         , ReleaseRoot, 12);
        false -> io:format("~p is not a directory~n", [ReleaseRoot])
    end.

walk_release(Verbose, _FileH, [], _Dir, _N) -> ok;
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
            FilePathNoRel = lists:foldl(fun
                            ("..", Acc) -> Acc;
                            ("rel", Acc) -> Acc;
                            (P,Acc) -> Acc ++ [P]
                        end, [],
                       filename:split(Dir)),
            FilePath = filename:join([".." | FilePathNoRel]++[F]),
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
    end;
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
get_id(_Verbose, undefined, Field, undefined) ->
    Id = "id_"++?H(Field),
    case dets:lookup(?TAB, Id) of
        [] ->
            Item = #item{id = Id, name = Field, guid = uuid()},
            ok = dets:insert(?TAB, Item),
            {ok, Item#item.guid};
        [#item{} = Item] -> {ok, Item#item.guid}
    end.
