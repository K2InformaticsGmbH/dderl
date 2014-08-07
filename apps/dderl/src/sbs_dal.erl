-module(sbs_dal).
-include("dderl.hrl").

-export([get_objects/5
        ,get_object/3]).


-spec get_objects({atom(), pid()}, binary(), binary(), binary(), integer()) -> list().
get_objects(Sess, Channel, Table, <<>>, Limit) ->
    get_objects(Sess, Channel, Table, <<"[]">>, Limit);
get_objects(Sess, Channel, Table, Key, Limit) ->
    FilteredKeys = read_and_filter(Sess, Channel, Key, Limit, fun_filter_by_table(Table)),
    read(Sess, Channel, FilteredKeys).

-spec get_object({atom(), pid()}, binary(), binary()) -> binary().
get_object(Sess, Channel, Key) ->
    ?Info("The get object request ~p", [{Sess, Channel, Key}]),
    [SingleResult | _Ignored] = read(Sess, Channel, Key),
    SingleResult.

-spec read({atom(), pid()}, binary(), binary()) -> list().
read(Sess, Channel, <<>>) ->
    ?Info("The read request ~p", [{Sess, Channel, <<>>}]),
    [];
read(Sess, Channel, Key) ->
    ?Info("The read request ~p", [{Sess, Channel, Key}]),
    {ok, Result} = Sess:run_cmd(dal_exec, [imem_dal_skvh, read, [Channel, <<"kvpair">>, Key]]),
    Result.

-spec read_and_filter({atom(), pid()}, binary(), binary(), integer(), fun()) -> binary().
read_and_filter(Sess, Channel, StartKey, Limit, KeyFilterFun) ->
    list_to_binary(string:join(read_and_filter(Sess, Channel, StartKey, Limit, KeyFilterFun, []), "\n")).

-spec read_and_filter({atom(), pid()}, binary(), binary(), integer(), fun(), list()) -> list().
read_and_filter(Sess, Channel, StartKey, Limit, KeyFilterFun, Acc) ->
    DoubleLimit = Limit * 2,
    {ok, All} = Sess:run_cmd(dal_exec, [imem_dal_skvh, readGT, [Channel, <<"key">>, StartKey, DoubleLimit]]),
    Filtered = Acc ++ [binary_to_list(Key) || Key <- All, KeyFilterFun(Key)],
    if
        length(Filtered) < Limit andalso length(All) =:= DoubleLimit ->
            read_and_filter(Sess, Channel, StartKey, Limit, KeyFilterFun, Filtered);
        true ->
            lists:sublist(Filtered, Limit)
    end.

-spec fun_filter_by_table(binary()) -> fun().
fun_filter_by_table(<<>>) ->
    fun(Key) -> length(imem_datatype:io_to_term(Key)) =:= 1 end;
fun_filter_by_table(<<"IPCON">>) ->
    fun(Key) ->
            case imem_datatype:io_to_term(Key) of
                [_, "IPCON", _] -> true;
                _ -> false
            end
    end.
