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

main([]) ->
    ?L("Building RPM from master tag silently"),
    Pwd = filename:dirname(filename:absname(escript:script_name())),
    [_,_|ProjectDirParts] = lists:reverse(filename:split(Pwd)),
    ProjDir = filename:join(lists:reverse(ProjectDirParts)),
    ?L("ScriptPath ~p~nProjectPath ~p", [Pwd, ProjDir]),
    {ok, CurDir} = file:get_cwd(),
    if CurDir /= Pwd -> ok = file:set_cwd(Pwd); true -> ok end,
    [begin
        ?OSCMD("rm -rf "++Dir),
        ok = file:make_dir(Dir)
    end || Dir <- ["BUILD", "BUILDROOT", "RPMS", "SOURCES", "SPECS", "SRPMS"]],

    % Package details
    {ok, [{application,dderl,DderlAppProps}]} =
        file:consult(filename:join([ProjDir,"src","dderl.app.src"])),
    Version = proplists:get_value(vsn, DderlAppProps, ""),

    % Build a source tarball
    build_sources(ProjDir, filename:join(Pwd, "SOURCES"), Version),

    % Build Spec
    Description = proplists:get_value(description, DderlAppProps, ""),
    SpecsFolder = filename:join(Pwd, "SPECS"),
    make_spec(ProjDir, SpecsFolder, Version, Description),

    ?L("Building rpm, this may take a while..."),
    ok = file:set_cwd(SpecsFolder),
    RpmBuildCmd = ?OSCMD("which rpmbuild"),
    run_port(RpmBuildCmd, ["-ba", "dderl.spec"], SpecsFolder),
    ok = file:set_cwd(CurDir).

run_port(Cmd, Args, Cwd) ->
    log_cmd(Cmd
           , erlang:open_port({spawn_executable, Cmd}
                             ,[{cd, Cwd},{line, 128},{args, Args}
                              ,exit_status,stderr_to_stdout
                              ,{parallelism, true}])).

-define(NL(__Fmt,__Args), io:format(__Fmt, __Args)).
-define(NL(__Fmt), ?NL(__Fmt,[])).
log_cmd(Cmd, Port) when is_port(Port) ->
    receive
        {'EXIT',Port,Reason} -> ?L("~s terminated for ~p", [Cmd, Reason]);
        {Port,closed} -> ?L("~s terminated", [Cmd]);
        {Port,{exit_status,Status}} ->
            ?L("~s exit with status ~p", [Cmd, Status]),
            catch erlang:port_close(Port);
        {Port,{data,{F,Line}}} ->
            ?NL("~s" ++ if F =:= eol -> "~n"; true -> "" end
               , [Line]),
            log_cmd(Cmd, Port);
        {Port,{data,Data}} ->
            ?NL("~p", [Data]),
            log_cmd(Cmd, Port)
    end.

build_sources(ProjDir, RpmSources, Version) ->
    RootDir = filename:join(RpmSources, "dderl-"++Version),
    ?OSCMD("rm -rf "++RootDir),
    ok = file:make_dir(RootDir),
    [begin
        {ok, _} = file:copy(filename:join(ProjDir,F),
                        filename:join(RootDir,F))
    end || F <- ["rebar.config", "LICENSE", "README.md", "RELEASE-DDERL.md"]],
    ?OSCMD("cp -L `which rebar` "++RootDir),
    copy_folder(ProjDir, RootDir, ["include"], "*.*"),
    copy_folder(ProjDir, RootDir, ["src"], "*.*"),
    copy_folder(ProjDir, RootDir, ["docs"], "*.*"),
    copy_folder(ProjDir, RootDir, ["rel"], "*.*"),
    copy_folder(ProjDir, RootDir, ["rel", "files"], "*"),
    copy_folder(ProjDir, RootDir, ["rel", "rpmbuild"], "*.*"),

    Priv = filename:join(RootDir, "priv"),
    ok = file:make_dir(Priv),
    copy_deep(filename:join([ProjDir, "priv"]), Priv),
    
    Deps = filename:join(RootDir, "deps"),
    ok = file:make_dir(Deps),
    copy_deep(filename:join([ProjDir, "deps"]), Deps),
    ?OSCMD("tar cvf "++filename:join(RpmSources, "dderl-"++Version++".tar.gz")
            ++" -C "++RpmSources++" dderl-"++Version),
    ?OSCMD("rm -rf "++RootDir).

copy_folder(Src, Target, Folder, Match) ->
    ok = file:make_dir(filename:join([Target|Folder])),
    [begin
        {ok, _} = file:copy(filename:join([Src|Folder]++[F]),
                            filename:join([Target|Folder]++[F]))
     end
     || F <- filelib:wildcard(Match, filename:join([Src|Folder]))].

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

make_spec(ProjDir, SpecPath, Version, Description) ->
    SpecFile = filename:join(SpecPath, "dderl.spec"),
    ?L("Writing Specs to ~s", [SpecFile]),

    % Description
    {ok, FileH} = file:open(SpecFile, [write, raw]),
    ok = file:write(FileH,
        "Name:           dderl\n"
        "Version:        "++Version++"\n"
        "Release:        1%{?dist}\n"
        "Summary:        "++Description++"\n"
        "\n"
        "Group:          Applications/Communications\n"
        "Vendor:         K2 Informatics, GmbH\n"
        "License:        Proprietary\n"
        "Packager:       Bikram Chatterjee <bikram@bluewin.ch>\n"
        "URL:            http://www.k2informatics.ch\n"
        "\n"
        "BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-"
                                                      "%(%{__id_u} -n)\n"
        "Source:         dderl-"++Version++".tar.gz\n"),

    % Description
    ok = file:write(FileH,
        "\n"
        "%description\n"),

    % No Debug package
    ErlVer = erlang:system_info(version),
    ok = file:write(FileH,
        "\n"
        "%define _topdir "++filename:dirname(filename:absname(escript:script_name()))++"\n"
        "%define _localstatedir /var/opt/dderl\n"
        "%define _bindir /opt/dderl/bin\n"
        "%define _sbindir /opt/dderl/bin\n"
        "%define _sysconfdir /etc\n"
        "%define _installdir /opt/dderl\n"
        "%define _reldir /opt/dderl/releases\n"
        "%define _erts /opt/dderl/erts-"++ErlVer++"\n"
        "%define _libdir /opt/dderl/lib\n"
        "%define _etcdir /opt/dderl/etc\n"
        "%define _config /opt/dderl/config\n"
        "%define _run /var/run/dderl\n"
        "%define _pipe /tmp/opt/dderl\n"
        "%define _smp_mflags  -j3\n"
        "%define __arch_install_post   /usr/lib/rpm/check-rpaths   /usr/lib/rpm/check-buildroot\n"
        "%define init_script %{_sysconfdir}/init.d/dderl\n"
        "%define debug_package %{nil}\n"
        "%global __prelink_undo_cmd %{nil}\n"),

    % Prep
    ok = file:write(FileH,
        "\n"
        "%prep\n"
        "%setup -q -n dderl-"++Version++"\n"),

    % Build
    ok = file:write(FileH,
        "\n"
        "%build\n"
        "./rebar compile\n"
        "./rebar generate skip_deps=true\n"),

    % Install
    ok = file:write(FileH,
        "\n"
        "%install\n"
        "%define relpath       %{_builddir}/%{buildsubdir}/rel/dderl\n"
        "%define buildroot_etc %{buildroot}%{_etcdir}\n"
        "\n"
        "mkdir -p %{buildroot_etc}\n"
        "mkdir -p %{buildroot}%{_libdir}\n"
        "mkdir -p %{buildroot}%{_reldir}\n"
        "mkdir -p %{buildroot}%{_erts}\n"
        "mkdir -p %{buildroot}%{_config}\n"
        "mkdir -p %{buildroot}%{_run}\n"
        "mkdir -p %{buildroot}%{_pipe}\n"
        "mkdir -p %{buildroot}%{_localstatedir}/log/dderl\n"
        "\n"
        "cp -R %{relpath}/etc       %{buildroot}%{_installdir}\n"
        "cp -R %{relpath}/lib       %{buildroot}%{_installdir}\n"
        "cp -R %{relpath}/erts-*    %{buildroot}%{_installdir}\n"
        "cp -R %{relpath}/releases  %{buildroot}%{_installdir}\n"
        "\n"
        "mkdir -p %{buildroot}%{_bindir}\n"
        "if [ -d %{relpath}/bin ]; then \\\n"
        "   find %{relpath}/bin -type f \\\n"
        "        -exec install -p -D -m 0755 {} %{buildroot}%{_bindir}/ \\; "
                                                                      ";fi\n"
        "\n"
        %"mkdir -p %{buildroot}%{_mandir}/man1\n"
        "if [ -d %{_builddir}/%{buildsubdir}/doc/man/man1 ]; then \\\n"
        "   echo -n; fi\n"
        "\n"
        "mkdir -p %{buildroot}%{_localstatedir}/log/dderl\n"
        "mkdir -p %{buildroot}%{_sysconfdir}/init.d\n"
        "install -m755 %{buildroot_etc}/init.script  %{buildroot}%{_sysconfdir}"
                                                                "/init.d/dderl\n"
        "\n"
        "# Needed to work around check-rpaths which seems to be hardcoded into"
        " recent\n"
        "# RPM releases\n"
        "export QA_RPATHS=3\n"),

    % Clean
    ok = file:write(FileH,
        "\n"
        "%clean\n"
        "rm -rf %{buildroot}\n"),

    % Files
    ok = file:write(FileH,
        "\n"
        "%files\n"
        "%defattr(-,dderl,dderl)\n"
        "%doc LICENSE\n"
        "%doc README.md\n"
        "%doc RELEASE-DDERL.md\n"
        "%{_bindir}\n"
        "%{_erts}/*\n"
        "%{_etcdir}/*\n"
        "%{_libdir}/*\n"
        "%{_reldir}/*\n"
        "%config(noreplace) %{_reldir}/"++Version++"/sys.config\n"
        "%config(noreplace) %{_reldir}/"++Version++"/vm.args\n"
        "%{_config}\n"
        "%{_run}\n"
        "%{_pipe}\n"
        "%{_localstatedir}/log/dderl\n"
        "%{_sysconfdir}/init.d/dderl\n"
        %"%{_mandir}/man1\n"
        "\n"),

    % Change Log
    ok = file:write(FileH,
        "%changelog\n"),

    % Pre Install
    ok = file:write(FileH,
        "\n"
        "%pre\n"
        "if ! getent group dderl >/dev/null 2>&1; then\n"
        "   groupadd -r dderl\n"
        "fi\n"
        "\n"
        "if getent passwd dderl >/dev/null 2>&1; then\n"
        "   usermod -d %{_localstatedir} dderl\n"
        "else\n"
        "   useradd -r -g dderl \\\n"
        "           --home %{_localstatedir} \\\n"
        "           --comment \"Dderl user\" \\\n"
        "           dderl\n"
        "fi\n"),

    % Pre Un-Install
    ok = file:write(FileH,
        "\n"
        "%preun\n"
        "chkconfig --del dderl\n"),

    % Post Install
    ok = file:write(FileH,
        "\n"
        "%post\n"
        "# Fixup perms for SELinux (if it is enabled)\n"
        "selinuxenabled && \\\n"
        "   find %{_localstatedir} -name \"*.so\" \\\n"
        "       -exec chcon -t textrel_shlib_t {} \\;\n"
        "\n"
        "# Softlinks to sys.config and vm.args for easy access\n"
        "if ! [ -L '%{_config}/sys.config' ]; then\n"
        "   ln -s %{_reldir}/"++Version++"/sys.config %{_config}/sys.config\n"
        "fi\n"
        "if ! [ -L '%{_config}/vm.args' ]; then\n"
        "ln -s %{_reldir}/"++Version++"/vm.args %{_config}/vm.args\n"
        "fi\n"
        "\n"
        "# Make sure shell library file is readable\n"
        "chmod 0755 %{_libdir}/env.sh\n"
        "chown -R dderl:dderl %{_installdir}\n"
        "chown -R dderl:dderl %{_localstatedir}\n"
        "chown -R dderl:dderl %{_run}\n"
        "chown -R mpro:mpro %{_pipe}\n"
        "chkconfig --add dderl\n"),

    ok = file:close(FileH).
