#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname build_msi -mnesia debug verbose

main([]) ->
    {Root, AppPath} = get_paths(),
    io:format("UUID ~s Root ~p AppPath ~p~n", [uuid(), Root, AppPath]),
    %make_soft_links(AppPath),
    %rebar_generate(Root),
    walk_release(Root);
main(_) ->
    usage().

usage() ->
    io:format("usage: build_msi.escript"),
    halt(1).

uuid() ->
    re:replace(os:cmd("uuidgen.exe")
              , "\r\n", "", [{return, binary}]).

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
        Cmd = lists:flatten(io_lib:format("mklink /D ~p ~p", [Link, Target])),
        io:format("~s~n~s~n", [Cmd, os:cmd(Cmd)])
     end
    || {Link, Target} <- [ {filename:join([AppPath, "src"]), "..\\..\\src"}
                          , {filename:join([AppPath, "priv"]), "..\\..\\priv"}]].

rebar_generate(Root) ->
    {ok, CurDir} = file:get_cwd(),
    ok = file:set_cwd(Root),
    io:format("Clean Compile and generate...~n", []),
    io:format("~s", [os:cmd("rebar clean")]),
    io:format("~s", [os:cmd("rebar compile")]),
    io:format("~s", [os:cmd("rebar generate")]),
    ok = file:set_cwd(CurDir).

walk_release(Root) ->
    ReleaseRoot = filename:join([Root,"rel","dderl"]),
    case filelib:is_dir(ReleaseRoot) of
        true ->
            walk_release(filelib:wildcard("*", ReleaseRoot), ReleaseRoot);
        false -> io:format("~p is not a directory~n", [ReleaseRoot])
    end.
walk_release([], _Dir) -> ok;
walk_release([F|Files], Dir) ->
    case filelib:is_dir(filename:join([Dir,F])) of
        true ->
            NewDirLevel = filename:join([Dir,F]),
            FilesAtThisLevel = filelib:wildcard("*", NewDirLevel),
            walk_release(FilesAtThisLevel, NewDirLevel);
        false ->
            io:format("~s has ~s~n", [Dir, F])
    end,
    walk_release(Files, Dir).

