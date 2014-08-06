-module(sbs_dal).
-include("dderl.hrl").

-export([get_objects/5
        ,get_object/3]).


-spec get_objects({atom(), pid()}, binary(), binary(), binary(), integer()) -> list().
get_objects(Sess, Channel, Table, <<>>, Limit) ->
    get_objects(Sess, Channel, Table, <<"[]">>, Limit);
get_objects(Sess, Channel, <<>>, Key, Limit) ->
    {ok, All} = Sess:run_cmd(dal_exec, [imem_dal_skvh, readGT, [Channel, <<"key">>, Key, Limit]]),
    Filtered = [binary_to_list(ToFilter) || ToFilter <- All, length(imem_datatype:io_to_term(ToFilter)) =:= 1],
    read(Sess, Channel, list_to_binary(string:join(Filtered, "\n")));
get_objects(Sess, Channel, _Table, Key, Limit) ->
    %% TODO: Filter by table.
    {ok, Result} = Sess:run_cmd(dal_exec, [imem_dal_skvh, readGT, [Channel, <<"key">>, Key, Limit]]),
    Result.

-spec get_object({atom(), pid()}, binary(), binary()) -> binary().
get_object(Sess, Channel, Key) ->
    ?Info("The get object request ~p", [{Sess, Channel, Key}]),
    [SingleResult | _Ignored] = read(Sess, Channel, Key),
    SingleResult.

-spec read({atom(), pid()}, binary(), binary()) -> list().
read(Sess, Channel, Key) ->
    ?Info("The read request ~p", [{Sess, Channel, Key}]),
    {ok, Result} = Sess:run_cmd(dal_exec, [imem_dal_skvh, read, [Channel, <<"kvpair">>, Key]]),
    Result.
