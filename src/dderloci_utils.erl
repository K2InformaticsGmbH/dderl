-module(dderloci_utils).

-export([oranumber_decode/1, oranumber_encode/1, ora_to_dderltime/1,
         dderltime_to_ora/1, apply_scale/2, clean_dynamic_prec/1, to_ora/2,
         ora_to_dderlts/1, dderlts_to_ora/1, ora_to_dderltstz/1, dderltstz_to_ora/1]).

-spec to_ora(atom(), any()) -> any().
to_ora('SQLT_INT', <<>>) -> <<>>;
to_ora('SQLT_FLT', <<>>) -> <<>>;
to_ora('SQLT_INT', V) -> binary_to_integer(V);
to_ora('SQLT_FLT', V) -> case catch binary_to_float(V) of
                             {'EXIT', _} ->
                                 _ = binary_to_integer(V),
                                 binary_to_float(<<V/binary,".0">>);
                             F -> F
                         end;
to_ora(T, V) when T=='SQLT_FLT'; T=='SQLT_INT'; T=='SQLT_UIN'; T=='SQLT_VNU';
                  T=='SQLT_NUM' -> oranumber_encode(V);
to_ora('SQLT_DAT', V) -> dderltime_to_ora(V);
to_ora('SQLT_TIMESTAMP', V) -> dderlts_to_ora(V);
to_ora('SQLT_TIMESTAMP_TZ', V) -> dderltstz_to_ora(V);
% all erlang-oci transparent types
to_ora(_T,V) -> V.

-spec oranumber_decode(binary()) -> {integer(), integer()} | {error, binary()}.
oranumber_decode(<<1:8, _/binary>>) -> {0, 0};
oranumber_decode(<<Length:8, 1:1, OraExp:7, Rest/binary>>) -> % positive numbers
    Exponent = OraExp - 65,
    MLength = Length - 1,
    <<OraMantissa:MLength/binary, _/binary>> = Rest,
    ListOraMant = binary_to_list(OraMantissa),
    ListMantissa = lists:flatten([io_lib:format("~2.10.0B", [DD-1]) || DD <- ListOraMant]),
    Mantissa = list_to_integer(ListMantissa),
    LengthMant = length(ListMantissa),
    oraexp_to_imem_prec(Mantissa, Exponent, LengthMant);
oranumber_decode(<<Length:8, 0:1, OraExp:7, Rest/binary>>) -> % negative numbers
    Exponent = 62 - OraExp,
    MLength = Length - 1,
    <<OraMantissa:MLength/binary, _/binary>> = Rest,
    ListOraMant = binary_to_list(OraMantissa),
    ListMantissa = lists:flatten([io_lib:format("~2.10.0B", [101-DD]) || DD <- ListOraMant, DD =/= 102]),
    Mantissa = -1 * list_to_integer(ListMantissa),
    LengthMant = length(ListMantissa),
    oraexp_to_imem_prec(Mantissa, Exponent, LengthMant);
oranumber_decode(_) -> {error, <<"invalid oracle number">>}.

-spec oraexp_to_imem_prec(integer(), integer(), integer()) -> {integer(), integer()}.
oraexp_to_imem_prec(Mantissa, Exponent, LengthMant) ->
    oraexp_to_imem_prec(Mantissa, Exponent, LengthMant, LengthMant rem 2, Mantissa rem 10).

-spec oraexp_to_imem_prec(integer(),integer(),integer(),integer(),integer()) -> {integer(), integer()}.
oraexp_to_imem_prec(Mantissa,Exponent,LengthMant,RemLength,0) -> {Mantissa div 10, (Exponent*-2) + LengthMant-3 + RemLength};
oraexp_to_imem_prec(Mantissa,Exponent,LengthMant,RemLength,_) -> {Mantissa       , (Exponent*-2) + LengthMant-2 + RemLength}.

-spec oranumber_encode(binary()) -> binary().
oranumber_encode(<<>>) -> <<>>;
oranumber_encode(NumberBin) ->
    {Mantissa, Exponent} = split_binary_number(NumberBin),
    oranumber_encode(Mantissa, Exponent).

-spec oranumber_encode(integer(), integer()) -> binary().
oranumber_encode(0, _) -> <<1, 128>>;
oranumber_encode(Mantissa, Exponent) when Mantissa > 0 ->
    NormMant = normalize_mantissa(Mantissa, Mantissa rem 100),
    Encoded = [Exponent + 193 | encode_mantissa(NormMant, 1, [])],
    list_to_binary([length(Encoded) | Encoded]);
oranumber_encode(Mantissa, Exponent) ->
    NormMant = normalize_mantissa(Mantissa, Mantissa rem 100),
    Encoded = [62 - Exponent | encode_mantissa(NormMant, 101, [102])],
    list_to_binary([length(Encoded) | Encoded]).

-spec encode_mantissa(integer(), 1 | 101, list()) -> list().
encode_mantissa(0, _, Result) -> Result;
encode_mantissa(Mantissa, Offset, Result) ->
    encode_mantissa(Mantissa div 100, Offset, [(Mantissa rem 100) + Offset | Result]).

-spec normalize_mantissa(integer(), integer()) -> integer().
normalize_mantissa(Mantissa, 0) ->
    NewMantissa = Mantissa div 100,
    normalize_mantissa(NewMantissa, NewMantissa rem 100);
normalize_mantissa(Mantissa, _Rest) -> Mantissa.

-spec split_binary_number(binary()) -> {integer(), integer()}.
split_binary_number(NumberBin) ->
    case remove_trailing_zeros(NumberBin) of
        {negative, CleanNumber} ->
            {Mantissa, Exponent} = parts_to_integer(binary:split(CleanNumber, [<<".">>], [])),
            {Mantissa * -1, Exponent};
        CleanNumber ->
            parts_to_integer(binary:split(CleanNumber, [<<".">>], []))
    end.

-spec parts_to_integer([binary()]) -> {integer(), integer(), integer()}.
parts_to_integer([<<>>]) -> {0, 0};
parts_to_integer([IntPart, <<>>]) -> parts_to_integer([IntPart]);
parts_to_integer([<<>>, RealPart]) -> parts_to_integer([<<"0">>, RealPart]);
parts_to_integer([IntPart]) ->
    SizeMant = size(IntPart),
    Exponent = (SizeMant div 2) + (SizeMant rem 2) - 1,
    Mantissa = binary_to_integer(IntPart),
    {Mantissa, Exponent};
parts_to_integer([<<"0">>, RealPart]) ->
    SizeMant = size(RealPart),
    %% Since the numbers are encoded 2 by 2 we need to add the last 0 to make the length even
    Mantissa = binary_to_integer(RealPart) * (1 +  9 * (SizeMant rem 2)),
    NewSizeMant = size(integer_to_binary(Mantissa)),
    Exponent = (NewSizeMant div 2) + (NewSizeMant rem 2) - (SizeMant div 2) - (SizeMant rem 2) - 1,
    {Mantissa, Exponent};
parts_to_integer([IntPart, RealPart]) ->
    SizeInt = size(IntPart),
    SizeReal = size(RealPart),
    Exponent = (SizeInt div 2) + (SizeInt rem 2) - 1,
    Mantissa = binary_to_integer(<<IntPart/binary, RealPart/binary>>) * (1 + 9 * (SizeReal rem 2)),
    {Mantissa, Exponent}.

-spec remove_trailing_zeros(binary()) -> binary() | {negative, binary()}.
remove_trailing_zeros(<<$-, RestNumber/binary>>) ->
    Clean = remove_trailing_zeros(RestNumber),
    {negative, Clean};
remove_trailing_zeros(OrigBin) ->
    LeftRemoved = lists:dropwhile(fun(X) -> X =:= $0 end, binary_to_list(OrigBin)),
    case lists:member($., LeftRemoved) of
        true ->
            RightRemoved = lists:dropwhile(fun(X) -> X =:= $0 end, lists:reverse(LeftRemoved)),
            list_to_binary(lists:reverse(RightRemoved));
        false ->
            list_to_binary(LeftRemoved)
    end.

-spec ora_to_dderltime(binary()) -> binary().
ora_to_dderltime(OraTime) ->
    imem_datatype:datetime_to_io(oci_util:from_dts(OraTime)).

-spec dderltime_to_ora(binary() | tuple()) -> binary().
dderltime_to_ora(<<>>) -> <<>>;
dderltime_to_ora(DDerlTime) when is_binary(DDerlTime) ->
    oci_util:to_dts(imem_datatype:io_to_datetime(DDerlTime));
dderltime_to_ora(DateTime) when is_tuple(DateTime) ->
    oci_util:to_dts(DateTime).

-spec ora_to_dderlts(binary()) -> binary().
ora_to_dderlts(<<>>) -> <<>>;
ora_to_dderlts(<<OraDateTime:7/binary, IntFracSecs:32>>) ->
    ListFracSecs = case integer_to_list(IntFracSecs) of
        NeedPad when length(NeedPad) < 9 ->
            lists:duplicate(9 - length(NeedPad), $0) ++ NeedPad;
        FullPrec -> FullPrec
    end, 
    case string:trim(ListFracSecs, trailing, "0") of
        [] -> ora_to_dderltime(OraDateTime);
        FracSecs -> iolist_to_binary([ora_to_dderltime(OraDateTime), $., FracSecs])
    end.

-spec dderlts_to_ora(binary() | tuple()) -> binary().
dderlts_to_ora(<<>>) -> <<>>;
dderlts_to_ora(Value) ->
    {DateTime, IntFracSecs} = case imem_datatype:io_to_timestamp(Value) of
        {_Secs, 0, _Node, _Seq} -> % Fractional second not present.
            {Value, 0};
        _ ->
            [DateTimeList, FracSecs] = string:split(binary_to_list(Value), ".", trailing),
            {list_to_binary(DateTimeList), list_to_integer(fix_length_frac_sec(FracSecs))}
    end,
    <<(dderltime_to_ora(DateTime)):7/binary, IntFracSecs:32>>.

-spec ora_to_dderltstz(binary()) -> binary().
ora_to_dderltstz(<<>>) -> <<>>;
ora_to_dderltstz(<<TS:11/binary, H, M>>) ->
    iolist_to_binary([ora_to_dderlts(TS), format_tz(H-20, M)]).

-spec dderltstz_to_ora(binary()) -> binary().
dderltstz_to_ora(<<>>) -> <<>>;
dderltstz_to_ora(Value) ->
    ValueList = binary_to_list(Value),
    case string:split(ValueList, "+", trailing) of
        [TS, Offset] ->
            <<(dderlts_to_ora(list_to_binary(TS))):11/binary, (parse_tz(Offset, positive)):2/binary>>;
        _ ->
            case string:split(ValueList, "-", trailing) of
                [TS, Offset] ->
                    <<(dderlts_to_ora(list_to_binary(TS))):11/binary, (parse_tz(Offset, negative)):2/binary>>;
                _ ->
                    throw("Optional timezone is not suported")
            end
    end.

-spec format_tz(integer(), integer()) -> list().
format_tz(TZOffset, M) when TZOffset > 0 ->
    [$+, pad_tz(TZOffset), integer_to_list(TZOffset), $:, pad_tz(M-60), integer_to_list(M-60)];
format_tz(TZOffset, M) when TZOffset =:= 0, M >= 60 ->
    [$+, pad_tz(TZOffset), integer_to_list(TZOffset), $:, pad_tz(M-60), integer_to_list(M-60)];
format_tz(TZOffset, M) ->
    AbsOffset = abs(TZOffset),
    [$-, pad_tz(AbsOffset), integer_to_list(AbsOffset), $:, pad_tz(60-M), integer_to_list(60-M)].

-spec pad_tz(integer()) -> [] | [$0].
pad_tz(TzDigit) when TzDigit < 10 -> [$0];
pad_tz(_) -> []. 

-spec parse_tz(list(), positive | negative) -> binary().
parse_tz(Offset, positive) ->
    {H, M} = parse_tz(Offset),
    <<(H+20):8, (60+M)>>;
parse_tz(Offset, negative) ->
    {H, M} = parse_tz(Offset),
    <<(20-H):8, (60-M)>>.

-spec parse_tz(list()) -> {integer(), integer()}.
parse_tz(Offset) ->
    case string:split(Offset, ":") of
        [HList, MList] ->
            H = list_to_integer(HList),
            M = list_to_integer(MList),
            case check_tz_range(H, M) of
                ok -> {H, M};
                invalid -> throw("Timezone range error -12:00 < TZ < 14:00")
            end;
        _ ->
            throw("Timezone format not supported")
    end.

-spec check_tz_range(integer(), integer()) -> ok | invalid.
check_tz_range(H, M) when H >= -12 andalso H < 14 andalso M >= 0 andalso M =< 59 -> ok;
check_tz_range(H, M) when H =:= 14 andalso M =:= 0 -> ok;
check_tz_range(_, _) -> invalid.

-spec fix_length_frac_sec(list()) -> list().
fix_length_frac_sec(FracSecs) when length(FracSecs) >= 9 ->
    string:slice(FracSecs, 0, 10);
fix_length_frac_sec(FracSecs) ->
    FracSecs ++ lists:duplicate(9 - length(FracSecs), $0).

-spec apply_scale(binary(), integer()) -> binary().
apply_scale(Value, Scale) ->
    case remove_trailing_zeros(Value) of
        {negative, CleanNumber} ->
            CleanNumber,
            case apply_scale_clean(binary:split(CleanNumber, [<<".">>], []), Scale) of
                <<"0">> -> <<"0">>;
                ResultPos -> <<$-, ResultPos/binary>>
            end;
        CleanNumber ->
            CleanNumber,
            apply_scale_clean(binary:split(CleanNumber, [<<".">>], []), Scale)
    end.

-spec apply_scale_clean([binary()], integer()) -> binary().
apply_scale_clean([<<>>], _Scale) -> <<"0">>;
apply_scale_clean([<<>>, <<>>], _Scale) -> <<"0">>;
apply_scale_clean([<<>>, RealPart], Scale) -> apply_scale_clean([<<"0">>, RealPart], Scale);
apply_scale_clean([IntPart, <<>>], Scale) -> apply_scale_clean([IntPart], Scale);
apply_scale_clean([IntPart], Scale) when Scale >= 0 -> IntPart;
apply_scale_clean([IntPart], Scale) ->
    ScalePos = Scale * -1,
    Factor = pow_bin(10, ScalePos),
    CutOff = Factor div 2,
    ValueAsInteger = binary_to_integer(IntPart),
    Base = ValueAsInteger div Factor,
    Remainder = ValueAsInteger rem Factor,
    if
        Remainder < CutOff ->
            integer_to_binary(Base * Factor);
        true ->
            integer_to_binary((Base + 1) * Factor)
    end;
apply_scale_clean([IntPart, _], Scale) when Scale =< 0 -> apply_scale_clean([IntPart], Scale);
apply_scale_clean([IntPart, RealPart], Scale) when Scale >= size(RealPart) -> <<IntPart/binary, $., RealPart/binary>>;
apply_scale_clean([IntPart, RealPart], Scale) ->
    DigitsToRemove = size(RealPart) - Scale,
    RealListReversed = lists:reverse(binary_to_list(RealPart)),
    {ToDiscard, RealFixedR} = lists:split(DigitsToRemove, RealListReversed),
    RealFixed = lists:reverse(RealFixedR),
    case lists:last(ToDiscard) < $5 of
        true ->
            iolist_to_binary([IntPart, $., RealFixed]);
        false ->
            SizeBeforeRound = length(RealFixed),
            Rounded = integer_to_binary(list_to_integer(RealFixed) + 1),
            SizeAfterRound = size(Rounded),
            if
                SizeAfterRound =:= SizeBeforeRound ->
                    <<IntPart/binary, $., Rounded/binary>>;
                SizeAfterRound < SizeBeforeRound ->
                    ZerosToAdd = SizeBeforeRound - SizeAfterRound,
                    iolist_to_binary([IntPart, $., lists:duplicate(ZerosToAdd, $0), Rounded]);
                true ->
                    integer_to_binary(binary_to_integer(IntPart) + 1)
            end
    end.

pow_bin(X, N) ->
    pow_bin(X, N, 1).

pow_bin(X, N, Acc) when (N rem 2) =:= 0 ->
    pow_bin(X * X, N div 2, Acc);
pow_bin(X, N, Acc) ->
    NewAcc = Acc * X,
    case N div 2 of
        0 ->
            NewAcc;
        _ ->
            pow_bin(X * X, N div 2, Acc * X)
    end.

-spec clean_dynamic_prec(binary()) -> binary().
clean_dynamic_prec(Number) ->
    case binary:split(Number, <<".">>) of
        [Int] -> Int;
        [Int, Frac] ->
            case string:strip(binary_to_list(Frac), right, $0) of
                [] -> Int;
                FracList ->
                    CleanFrac = list_to_binary(FracList),
                    <<Int/binary, $., CleanFrac/binary>>
            end
    end.
