-module(dderloci_utils_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

oranumber_decode_positive_int_test_() ->
    [
        ?_assertEqual({0, 0}, dderloci_utils:oranumber_decode(<<1, 128>>)), % num 0
        ?_assertEqual({5, 0}, dderloci_utils:oranumber_decode(<<2, 193, 6>>)), % num 5
        ?_assertEqual({1, -4}, dderloci_utils:oranumber_decode(<<2, 195, 2>>)), % num 10000
        ?_assertEqual({32, 0}, dderloci_utils:oranumber_decode(<<2, 193, 33>>)), % num 32
        ?_assertEqual({1, -5}, dderloci_utils:oranumber_decode(<<2, 195, 11>>)), % num 100000
        ?_assertEqual({2767, 0}, dderloci_utils:oranumber_decode(<<3, 194, 28, 68>>)) % num 2767
    ].

oranumber_decode_negative_int_test_() ->
    [
        ?_assertEqual({-5, 0}, dderloci_utils:oranumber_decode(<<3, 62, 96, 102>>)), % num -5
        ?_assertEqual({-112, 0}, dderloci_utils:oranumber_decode(<<4, 61, 100, 89, 102>>)), % num -112
        ?_assertEqual({-2767, 0}, dderloci_utils:oranumber_decode(<<4, 61, 74, 34, 102>>)), % num -2767
        ?_assertEqual({-11, -4}, dderloci_utils:oranumber_decode(<<3, 60, 90, 102>>)), % num -110000
        ?_assertEqual({-9999, -4}, dderloci_utils:oranumber_decode(<<4, 59, 2, 2, 102>>)), % -99990000
        ?_assertEqual({-1000001, 0}, dderloci_utils:oranumber_decode(<<6, 59, 100, 101, 101, 100, 102>>)), % num -1000001
        ?_assertEqual({-1, -6}, dderloci_utils:oranumber_decode(<<3, 59, 100, 102>>)) % num -1000000
    ].

oranumber_decode_positive_real_test_() ->
    [
        ?_assertEqual({1, 1}, dderloci_utils:oranumber_decode(<<2, 192, 11>>)), % num 0.1
        ?_assertEqual({1, 6}, dderloci_utils:oranumber_decode(<<2, 190, 2>>)), % num 0.000001
        ?_assertEqual({333122, 6}, dderloci_utils:oranumber_decode(<<4, 192, 34, 32, 23>>)), % num 0.333122
        ?_assertEqual({55, 1}, dderloci_utils:oranumber_decode(<<3, 193, 6, 51>>)), % num 5.5
        ?_assertEqual({105, 1}, dderloci_utils:oranumber_decode(<<3, 193, 11, 51>>)), % num 10.5
        ?_assertEqual({1005, 1}, dderloci_utils:oranumber_decode(<<4, 194, 2, 1, 51>>)), % num 100.5
        ?_assertEqual({10000000000000001, 9}, dderloci_utils:oranumber_decode(<<10, 196, 11, 1, 1, 1, 1, 1, 1, 1, 11>>)), % num 10000000.000000001
        ?_assertEqual({3223059983, 5}, dderloci_utils:oranumber_decode(<<7, 195, 4, 23, 31, 60, 99, 31>>)) % num 32230.59983
    ].

oranumber_decode_negative_real_test_() ->
    [
        ?_assertEqual({-1, 1}, dderloci_utils:oranumber_decode(<<3, 63, 91, 102>>)), % num -0.1
        ?_assertEqual({-1, 6}, dderloci_utils:oranumber_decode(<<3, 65, 100, 102>>)), % num -0.000001
        ?_assertEqual({-333122, 6}, dderloci_utils:oranumber_decode(<<5, 63, 68, 70, 79, 102>>)), % num -0.333122
        ?_assertEqual({-55, 1}, dderloci_utils:oranumber_decode(<<4, 62, 96, 51, 102>>)), % num -5.5
        ?_assertEqual({-105, 1}, dderloci_utils:oranumber_decode(<<4, 62, 91, 51, 102>>)) % num -10.5
    ].

oranumber_encode_positive_int_test_() ->
    [
        ?_assertEqual(<<>>, dderloci_utils:oranumber_encode(<<>>)),
        ?_assertEqual(<<1, 128>>, dderloci_utils:oranumber_encode(<<"0">>)),
        ?_assertEqual(<<2, 193, 6>>, dderloci_utils:oranumber_encode(<<"5">>)),
        ?_assertEqual(<<2, 193, 6>>, dderloci_utils:oranumber_encode(<<"005">>)),
        ?_assertEqual(<<2, 195, 2>>, dderloci_utils:oranumber_encode(<<"10000">>)),
        ?_assertEqual(<<2, 195, 2>>, dderloci_utils:oranumber_encode(<<"0010000">>)),
        ?_assertEqual(<<2, 193, 33>>, dderloci_utils:oranumber_encode(<<"32">>)),
        ?_assertEqual(<<2, 193, 33>>, dderloci_utils:oranumber_encode(<<"32.">>)),
        ?_assertEqual(<<2, 195, 11>>, dderloci_utils:oranumber_encode(<<"100000">>)),
        ?_assertEqual(<<3, 194, 28, 68>>, dderloci_utils:oranumber_encode(<<"2767">>))
    ].

oranumber_encode_negative_int_test_() ->
    [
        ?_assertEqual(<<1, 128>>, dderloci_utils:oranumber_encode(<<"-0">>)),
        ?_assertEqual(<<3, 62, 96, 102>>, dderloci_utils:oranumber_encode(<<"-5">>)),
        ?_assertEqual(<<3, 62, 96, 102>>, dderloci_utils:oranumber_encode(<<"-005">>)),
        ?_assertEqual(<<3, 62, 96, 102>>, dderloci_utils:oranumber_encode(<<"-5.0">>)),
        ?_assertEqual(<<4, 61, 100, 89, 102>>, dderloci_utils:oranumber_encode(<<"-112">>)),
        ?_assertEqual(<<4, 61, 74, 34, 102>>, dderloci_utils:oranumber_encode(<<"-2767">>)),
        ?_assertEqual(<<3, 60, 90, 102>>, dderloci_utils:oranumber_encode(<<"-110000">>)),
        ?_assertEqual(<<4, 59, 2, 2, 102>>, dderloci_utils:oranumber_encode(<<"-99990000">>)),
        ?_assertEqual(<<6, 59, 100, 101, 101, 100, 102>>, dderloci_utils:oranumber_encode(<<"-1000001">>)),
        ?_assertEqual(<<3, 59, 100, 102>>, dderloci_utils:oranumber_encode(<<"-1000000">>))
    ].

oranumber_encode_positive_real_test_() ->
    [
        ?_assertEqual(<<1, 128>>, dderloci_utils:oranumber_encode(<<"0.0">>)),
        ?_assertEqual(<<2, 192, 11>>, dderloci_utils:oranumber_encode(<<"0.1">>)),
        ?_assertEqual(<<2, 192, 11>>, dderloci_utils:oranumber_encode(<<".1">>)),
        ?_assertEqual(<<2, 192, 11>>, dderloci_utils:oranumber_encode(<<".100">>)),
        ?_assertEqual(<<2, 190, 2>>, dderloci_utils:oranumber_encode(<<"0.000001">>)),
        ?_assertEqual(<<2, 190, 2>>, dderloci_utils:oranumber_encode(<<"0000.000001">>)),
        ?_assertEqual(<<4, 192, 34, 32, 23>>, dderloci_utils:oranumber_encode(<<"0.333122">>)),
        ?_assertEqual(<<3, 193, 6, 51>>, dderloci_utils:oranumber_encode(<<"5.5">>)),
        ?_assertEqual(<<3, 193, 11, 51>>, dderloci_utils:oranumber_encode(<<"10.5">>)),
        ?_assertEqual(<<4, 194, 2, 1, 51>>, dderloci_utils:oranumber_encode(<<"100.5">>)),
        ?_assertEqual(<<10, 196, 11, 1, 1, 1, 1, 1, 1, 1, 11>>, dderloci_utils:oranumber_encode(<<"10000000.000000001">>)),
        ?_assertEqual(<<7, 195, 4, 23, 31, 60, 99, 31>>, dderloci_utils:oranumber_encode(<<"32230.59983">>))
    ].

oranumber_encode_negative_real_test_() ->
    [
        ?_assertEqual(<<1, 128>>, dderloci_utils:oranumber_encode(<<"-0.0">>)),
        ?_assertEqual(<<1, 128>>, dderloci_utils:oranumber_encode(<<"-.0">>)),
        ?_assertEqual(<<3, 63, 91, 102>>, dderloci_utils:oranumber_encode(<<"-0.1">>)),
        ?_assertEqual(<<3, 63, 91, 102>>, dderloci_utils:oranumber_encode(<<"-.1">>)),
        ?_assertEqual(<<3, 63, 91, 102>>, dderloci_utils:oranumber_encode(<<"-.1000">>)),
        ?_assertEqual(<<3, 65, 100, 102>>, dderloci_utils:oranumber_encode(<<"-0.000001">>)),
        ?_assertEqual(<<3, 65, 100, 102>>, dderloci_utils:oranumber_encode(<<"-0000.000001">>)),
        ?_assertEqual(<<5, 63, 68, 70, 79, 102>>, dderloci_utils:oranumber_encode(<<"-0.333122">>)),
        ?_assertEqual(<<4, 62, 96, 51, 102>>, dderloci_utils:oranumber_encode(<<"-5.5">>)),
        ?_assertEqual(<<4, 62, 91, 51, 102>>, dderloci_utils:oranumber_encode(<<"-10.5">>)),
        ?_assertEqual(<<5, 61, 100, 101, 51, 102>>, dderloci_utils:oranumber_encode(<<"-100.5">>)),
        ?_assertEqual(<<11, 59, 91, 101, 101, 101, 101, 101, 101, 101, 91, 102>>, dderloci_utils:oranumber_encode(<<"-10000000.000000001">>)),
        ?_assertEqual(<<8, 60, 98, 79, 71, 42, 3, 71, 102>>, dderloci_utils:oranumber_encode(<<"-32230.59983">>))
    ].

apply_scale_positive_test_() ->
    [
        ?_assertEqual(<<"0">>, dderloci_utils:apply_scale(<<"0.0">>, 0)),
        ?_assertEqual(<<"0">>, dderloci_utils:apply_scale(<<"0.33">>, 0)),
        ?_assertEqual(<<"2">>, dderloci_utils:apply_scale(<<"2.33">>, 0)),
        ?_assertEqual(<<"2">>, dderloci_utils:apply_scale(<<"2.0">>, 1)),
        ?_assertEqual(<<"33.8">>, dderloci_utils:apply_scale(<<"33.8">>, 2)),
        ?_assertEqual(<<"33.8">>, dderloci_utils:apply_scale(<<"33.78">>, 1)),
        ?_assertEqual(<<"33.78">>, dderloci_utils:apply_scale(<<"33.78">>, 2)),
        ?_assertEqual(<<"33.78">>, dderloci_utils:apply_scale(<<"33.7800">>, 2)),
        ?_assertEqual(<<"33.78">>, dderloci_utils:apply_scale(<<"33.7800">>, 4)),
        ?_assertEqual(<<"0.0036">>, dderloci_utils:apply_scale(<<"0.00355">>, 4)),
        ?_assertEqual(<<"0.0000485">>, dderloci_utils:apply_scale(<<"0.0000485">>, 10)),
        ?_assertEqual(<<"99">>, dderloci_utils:apply_scale(<<"98.9999">>, 3)),

        ?_assertEqual(<<"0">>, dderloci_utils:apply_scale(<<"-0.0">>, 0)),
        ?_assertEqual(<<"0">>, dderloci_utils:apply_scale(<<"-0.33">>, 0)),
        ?_assertEqual(<<"-2">>, dderloci_utils:apply_scale(<<"-2.33">>, 0)),
        ?_assertEqual(<<"-2">>, dderloci_utils:apply_scale(<<"-2.0">>, 1)),
        ?_assertEqual(<<"-33.8">>, dderloci_utils:apply_scale(<<"-33.8">>, 2)),
        ?_assertEqual(<<"-33.8">>, dderloci_utils:apply_scale(<<"-33.78">>, 1)),
        ?_assertEqual(<<"-33.78">>, dderloci_utils:apply_scale(<<"-33.78">>, 2)),
        ?_assertEqual(<<"-33.78">>, dderloci_utils:apply_scale(<<"-33.7800">>, 2)),
        ?_assertEqual(<<"-33.78">>, dderloci_utils:apply_scale(<<"-33.7800">>, 4)),
        ?_assertEqual(<<"-0.0036">>, dderloci_utils:apply_scale(<<"-0.00355">>, 4)),
        ?_assertEqual(<<"-0.0000485">>, dderloci_utils:apply_scale(<<"-0.0000485">>, 10)),
        ?_assertEqual(<<"-99">>, dderloci_utils:apply_scale(<<"-98.9999">>, 3))
    ].

apply_scale_negative_test_() ->
    [
        ?_assertEqual(<<"50">>, dderloci_utils:apply_scale(<<"50">>, -1)),
        ?_assertEqual(<<"0">>, dderloci_utils:apply_scale(<<"23">>, -2)),
        ?_assertEqual(<<"100">>, dderloci_utils:apply_scale(<<"123">>, -2)),
        ?_assertEqual(<<"1100">>, dderloci_utils:apply_scale(<<"1123">>, -2)),
        ?_assertEqual(<<"1100">>, dderloci_utils:apply_scale(<<"1100">>, -2)),
        ?_assertEqual(<<"1200">>, dderloci_utils:apply_scale(<<"1153">>, -2)),
        ?_assertEqual(<<"1200">>, dderloci_utils:apply_scale(<<"1153.44332">>, -2)),
        ?_assertEqual(<<"0">>, dderloci_utils:apply_scale(<<"0.00355">>, -2)),

        ?_assertEqual(<<"-50">>, dderloci_utils:apply_scale(<<"-50">>, -1)),
        ?_assertEqual(<<"0">>, dderloci_utils:apply_scale(<<"-23">>, -2)),
        ?_assertEqual(<<"-100">>, dderloci_utils:apply_scale(<<"-123">>, -2)),
        ?_assertEqual(<<"-1100">>, dderloci_utils:apply_scale(<<"-1123">>, -2)),
        ?_assertEqual(<<"-1100">>, dderloci_utils:apply_scale(<<"-1100">>, -2)),
        ?_assertEqual(<<"-1200">>, dderloci_utils:apply_scale(<<"-1153">>, -2)),
        ?_assertEqual(<<"-1200">>, dderloci_utils:apply_scale(<<"-1153.44332">>, -2)),
        ?_assertEqual(<<"0">>, dderloci_utils:apply_scale(<<"-0.00355">>, -2))
    ].

clean_dynamic_prec_test_() ->
    [
        ?_assertEqual(<<"50">>, dderloci_utils:clean_dynamic_prec(<<"50">>)),
        ?_assertEqual(<<"0">>, dderloci_utils:clean_dynamic_prec(<<"0">>)),
        ?_assertEqual(<<"000">>, dderloci_utils:clean_dynamic_prec(<<"000">>)),
        ?_assertEqual(<<"0">>, dderloci_utils:clean_dynamic_prec(<<"0.0">>)),
        ?_assertEqual(<<"0.001">>, dderloci_utils:clean_dynamic_prec(<<"0.001">>)),
        ?_assertEqual(<<"0.002">>, dderloci_utils:clean_dynamic_prec(<<"0.00200">>)),
        ?_assertEqual(<<"0.5432">>, dderloci_utils:clean_dynamic_prec(<<"0.5432">>)),
        ?_assertEqual(<<"0.5">>, dderloci_utils:clean_dynamic_prec(<<"0.500">>)),
        ?_assertEqual(<<"-50">>, dderloci_utils:clean_dynamic_prec(<<"-50">>)),
        ?_assertEqual(<<"-0.001">>, dderloci_utils:clean_dynamic_prec(<<"-0.001">>)),
        ?_assertEqual(<<"-0.002">>, dderloci_utils:clean_dynamic_prec(<<"-0.00200">>)),
        ?_assertEqual(<<"-0.5432">>, dderloci_utils:clean_dynamic_prec(<<"-0.5432">>)),
        ?_assertEqual(<<"-0.5">>, dderloci_utils:clean_dynamic_prec(<<"-0.500">>)),
        ?_assertEqual(<<"230">>, dderloci_utils:clean_dynamic_prec(<<"230.0">>)),
        ?_assertEqual(<<"-33.49">>, dderloci_utils:clean_dynamic_prec(<<"-33.490">>))
    ].

timestamp_test_() ->
    case os:getenv("NO_OCI") of
        "true" -> [];
        _ -> [
            %% Encode
            ?_assertEqual(<<120,117,1,8,11,6,39,0,0,0,0>>, dderloci_utils:dderlts_to_ora(<<"08.01.2017 10:05:38">>)),
            ?_assertEqual(<<120,117,5,18,11,6,39,5,245,225,0>>, dderloci_utils:dderlts_to_ora(<<"18.05.2017 10:05:38.1">>)),
            ?_assertEqual(<<120,118,10,18,11,6,39,7,84,212,192>>, dderloci_utils:dderlts_to_ora(<<"18.10.2018 10:05:38.123">>)),
            ?_assertEqual(<<120,117,8,8,11,6,39,0,152,150,128>>, dderloci_utils:dderlts_to_ora(<<"08.08.2017 10:05:38.01">>)),
            ?_assertEqual(<<120,117,8,8,9,6,39,6,127,53,64>>, dderloci_utils:dderlts_to_ora(<<"08.08.2017 08:05:38.109000">>)),
            ?_assertEqual(<<120,117,8,8,6,6,39,0,137,84,64>>, dderloci_utils:dderlts_to_ora(<<"08.08.2017 05:05:38.009">>)),
            ?_assertEqual(<<120,117,8,8,11,6,39,0,137,84,64>>, dderloci_utils:dderlts_to_ora(<<"08.08.2017 10:05:38.009000">>)),
            ?_assertEqual(<<120,117,8,8,11,6,39,0,0,3,232>>, dderloci_utils:dderlts_to_ora(<<"08.08.2017 10:05:38.000001">>)),
            ?_assertEqual(<<120,117,8,8,11,6,39,7,91,205,21>>, dderloci_utils:dderlts_to_ora(<<"08.08.2017 10:05:38.123456789">>)),
            ?_assertEqual(<<120,117,8,8,9,6,39,59,154,198,24>>, dderloci_utils:dderlts_to_ora(<<"08.08.2017 08:05:38.999999">>)),

            %% Decode
            ?_assertEqual(<<"08.01.2017 10:05:38">>, dderloci_utils:ora_to_dderlts(<<120,117,1,8,11,6,39,0,0,0,0>>)),
            ?_assertEqual(<<"18.05.2017 10:05:38.1">>, dderloci_utils:ora_to_dderlts(<<120,117,5,18,11,6,39,5,245,225,0>>)),
            ?_assertEqual(<<"18.10.2018 10:05:38.123">>, dderloci_utils:ora_to_dderlts(<<120,118,10,18,11,6,39,7,84,212,192>>)),
            ?_assertEqual(<<"08.08.2017 10:05:38.01">>, dderloci_utils:ora_to_dderlts(<<120,117,8,8,11,6,39,0,152,150,128>>)),
            ?_assertEqual(<<"08.08.2017 08:05:38.109">>, dderloci_utils:ora_to_dderlts(<<120,117,8,8,9,6,39,6,127,53,64>>)),
            ?_assertEqual(<<"08.08.2017 05:05:38.009">>, dderloci_utils:ora_to_dderlts(<<120,117,8,8,6,6,39,0,137,84,64>>)),
            ?_assertEqual(<<"08.08.2017 10:05:38.009">>, dderloci_utils:ora_to_dderlts(<<120,117,8,8,11,6,39,0,137,84,64>>)),
            ?_assertEqual(<<"08.08.2017 10:05:38.000001">>, dderloci_utils:ora_to_dderlts(<<120,117,8,8,11,6,39,0,0,3,232>>)),
            ?_assertEqual(<<"08.08.2017 10:05:38.123456789">>, dderloci_utils:ora_to_dderlts(<<120,117,8,8,11,6,39,7,91,205,21>>)),
            ?_assertEqual(<<"08.08.2017 08:05:38.999999">>, dderloci_utils:ora_to_dderlts(<<120,117,8,8,9,6,39,59,154,198,24>>))
        ]
    end.

-endif.
