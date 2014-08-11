#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname build_msi -mnesia debug verbose

-define(PRODUCT_GUID, "80D1D993-AAAD-4F38-A5EE-84428476E69F").
-define(UPGRADE_GUID, "C20627C5-BE87-471D-BC11-3F63E6BF09B8").
-define(COMPANY, "K2 Informatics GmbH").
-define(PRODUCT, "DDErl").
-define(VERSION, "1.0.7").
-define(PKG_COMMENT, "DDErl is a registered trademark of"
                     " K2 Informatics GmbH").
-define(WXSFILE, "dderl.wxs").
-define(WIXOBJFILE, "dderl.wixobj").
-define(TAB, "dderlids").
-define(TABFILE, "dderlids.dets").

main([]) ->
    {Root, AppPath} = get_paths(),
    io:format("UUID ~s Root ~p AppPath ~p~n", [uuid(), Root, AppPath]),
    %make_soft_links(AppPath),
    %rebar_generate(Root),
    {ok, ?TAB} = dets:open_file(?TAB, [{ram_file, true}]),
    create_wxs(Root),
    true = dets:delete(?TAB);
main(_) ->
    usage().

usage() ->
    io:format("usage: build_msi.escript"),
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

walk_release(FileH, Root) ->
    ReleaseRoot = filename:join([Root,"rel","dderl"]),
    case filelib:is_dir(ReleaseRoot) of
        true ->
            walk_release(FileH, filelib:wildcard("*", ReleaseRoot)
                         , ReleaseRoot, 12);
        false -> io:format("~p is not a directory~n", [ReleaseRoot])
    end.

walk_release(_FileH, [], _Dir, _N) -> ok;
walk_release(FileH, [F|Files], Dir, N) ->
    case filelib:is_dir(filename:join([Dir,F])) of
        true ->
            NewDirLevel = filename:join([Dir,F]),
            FilesAtThisLevel = filelib:wildcard("*", NewDirLevel),
            ok = file:write(FileH, lists:duplicate(N,32)++
                            "<Directory Id='"++to_id(F)++
                                            "' Name='"++F++"'>\n"),
            walk_release(FileH, FilesAtThisLevel, NewDirLevel, N+3),
            ok = file:write(FileH, lists:duplicate(N,32)++"</Directory>\n");
        false ->
            FilePathNoRel = lists:foldl(fun
                            ("..", Acc) -> Acc;
                            ("rel", Acc) -> Acc;
                            (P,Acc) -> Acc ++ [P]
                        end, [],
                       filename:split(Dir)),
            FilePath = filename:join(FilePathNoRel++[F]),
            ok = file:write(FileH, lists:duplicate(N+3,32)++
                "<Component Id='"++to_id(F)++"' Guid='"++uuid()++"'>\n"
                ++lists:duplicate(N+3,32)++
                "   <File Id='"++to_id(F)++"' Name='"++F++
                                "' DiskId='1' Source='"++FilePath++"'"
                " KeyPath='yes' />\n"++lists:duplicate(N+3,32)++
                "</Component>\n"),
            io:format("File ~s~n"
                      , [FilePath])
    end,
    walk_release(FileH, Files, Dir, N).

create_wxs(Root) ->
    {ok, FileH} = file:open(filename:join([Root, "rel", ?WXSFILE])
                            , [write, raw]),
    ok = file:write(FileH,
        "<?xml version='1.0' encoding='windows-1252'?>\n"
        "<Wix xmlns='http://schemas.microsoft.com/wix/2006/wi'>\n\n"

        "<Product Name='DDErl 1.0.7'\n"
        "         Id='"?PRODUCT_GUID"'\n"
        "         UpgradeCode='"?UPGRADE_GUID"'\n"
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
        "       <Directory Id='"++to_id(?COMPANY)++"' Name='"?COMPANY"'>\n"
        "         <Directory Id='INSTALLDIR' Name='"?PRODUCT
                                            " "?VERSION"'>\n"),

    walk_release(FileH, Root),

    ok = file:write(FileH,
        "         </Directory>\n"
        "       </Directory>\n"
        "     </Directory>\n"
        "   </Directory>\n\n"

        "</Product>\n\n"
        "</Wix>"),
    ok = file:close(FileH),
    {ok, CurDir} = file:get_cwd(),
    ok = file:set_cwd(filename:join([Root,"rel"])),
    io:format("~s", [os:cmd("candle.exe "?WXSFILE)]),
    io:format("~s", [os:cmd("light.exe "?WIXOBJFILE)]),
    ok = file:set_cwd(CurDir).

to_id(V) ->
    Id = "id_"++re:replace(V,"[^A-Za-z0-9_.]","",[global,{return,list}]),
    make_unique_id(Id).

make_unique_id(Id) ->
    case dets:lookup(?TAB, Id) of
        [] ->
            true = dets:insert(?TAB, {Id, 1}),
            Id;
        _ -> make_unique_id(
               Id++integer_to_list(random:uniform(100)))
    end.
