%% -----------------------------------------------------------------------------
%%
%% sqlparse_params_filter_test.erl: SQL - test driver.
%%
%% Copyright (c) 2012-18 K2 Informatics GmbH.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

-module(sqlparse_params_filter_test).

-define(NODEBUG, true).

-include("sqlparse_params_filter_test.hrl").

%%------------------------------------------------------------------------------
%% Testing.
%%------------------------------------------------------------------------------

params_1_test_() ->
    ?D("Start ~n"),
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {params_filter("TEST_01", ?TEST_01, ?TEST_01_RESULT, [])},
                {params_filter("TEST_02", ?TEST_02, ?TEST_02_RESULT, [])},
                {params_filter("TEST_03", ?TEST_03, ?TEST_03_RESULT, [])}
            ]
        end
    }.

params_2_test_() ->
    ?D("Start ~n"),
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {params_filter("TEST_51", ?TEST_51, ?TEST_51_RESULT, ?TYPES)},
                {params_filter("TEST_52", ?TEST_52, ?TEST_52_RESULT, ?TYPES)},
                {params_filter("TEST_53", ?TEST_53, ?TEST_53_RESULT, ?TYPES)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Helper functions.
%%------------------------------------------------------------------------------

params_filter(Title, Source, Result, Types) ->
    ?D("Start ~n Title: ~p~n Source: ~p~n Result: ~p~n Types: ~p~n",
        [Title, Source, Result, Types]),
    {ok, ParseTree} = sqlparse:parsetree(Source),
    ?D("~n ParseTree: ~p~n", [ParseTree]),
    case sqlparse_fold:top_down(sqlparse_params_filter, ParseTree, Types) of
        Params when is_list(Params) -> ?assertEqual(Result, Params, Title);
        ErrorResult ->
            io:format(user, "~n" ++ ?MODULE_STRING ++
                " : Error in eunit_test : Title      ~n > ~p~n", [Title]),
            io:format(user, "~n" ++ ?MODULE_STRING ++
                " : Error in eunit_test : ErrorResult~n > ~p~n", [ErrorResult])
    end.

%%------------------------------------------------------------------------------
%% Setup functions.
%%------------------------------------------------------------------------------

setup_default() ->
    ?D("Start ~n"),
    ok.
