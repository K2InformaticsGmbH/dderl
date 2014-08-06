-module(sbs_dal).
-include("dderl.hrl").

-export([get_objects/5]).


-spec get_objects({atom(), pid()}, binary(), binary(), binary(), integer()) -> list().
get_objects(Sess, Channel, Table, <<>>, Limit) ->
    get_objects(Sess, Channel, Table, <<"[]">>, Limit);
get_objects(Sess, Channel, <<>>, Key, Limit) ->
    {ok, All} = Sess:run_cmd(dal_exec, [imem_dal_skvh, readGT, [Channel, <<"key">>, Key, Limit]]),
    Filtered = [binary_to_list(ToFilter) || ToFilter <- All, length(imem_datatype:io_to_term(ToFilter)) =:= 1],
    FilteredNewLine = list_to_binary(string:join(Filtered, "\n")),
    {ok, Result} = Sess:run_cmd(dal_exec, [imem_dal_skvh, read, [Channel, <<"kvpair">>, FilteredNewLine]]),
    Result;
get_objects(Sess, Channel, Table, Key, Limit) ->
    %% TODO: Filter by table.
    {ok, Result} = Sess:run_cmd(dal_exec, [imem_dal_skvh, readGT, [Channel, <<"key">>, Key, Limit]]),
    Result.
