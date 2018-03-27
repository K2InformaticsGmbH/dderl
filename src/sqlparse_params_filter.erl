%% -----------------------------------------------------------------------------
%%
%% sqlparse_params_filter.erl: SQL - filtering the parameters
%%                                   of a SQL statement.
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

-module(sqlparse_params_filter).

-export([
    finalize/2,
    fold/5,
    init/1
]).

-define(NODEBUG, true).

-include_lib("sqlparse/include/sqlparse_fold.hrl").

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Setting up parameters.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(Types :: [binary()]) -> [].
init(Types) ->
    ?D("Start~n Types: ~p~n", [Types]),
    MP = case Types of
             [] -> [];
             _ -> {ok, MP_I} = re:compile("[^a-zA-Z0-9() =><]*:(" ++
                 string:join([binary_to_list(T) || T <- Types], "|") ++
                 ")((_IN_|_OUT_|_INOUT_){0,1})[^ ,\)\n\r;]+"),
                 MP_I
         end,
    ?D("end~n MP: ~p~n", [MP]),
    MP.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Postprocessing of the result.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec finalize(Params :: any(), Ctx :: [binary()]|tuple()) -> Ctx :: [binary()]|tuple().
finalize(_MP, CtxIn)
    when is_list(CtxIn) ->
    ?D("Start~n MP: ~p~n CtxIn: ~p~n", [_MP, CtxIn]),
    CtxOut = lists:usort(CtxIn),
    ?D("~n CtxOut: ~p~n", [CtxOut]),
    CtxOut.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Layout method for processing the various parser subtrees
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec fold(string(), FunState :: tuple(), Ctx :: [binary()],
    PTree :: list()|tuple(), FoldState :: tuple()) -> Ctx :: list().

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% param
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(MP, _FunState, Ctx, {param, Param} = _PTree, FoldState)
    when element(2, FoldState) == start ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, FoldState),
    RT = add_param(MP, Param, Ctx),
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% {param, _}
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(MP, _FunState, Ctx, {{param, Param}, _} = _PTree, FoldState)
    when element(2, FoldState) == start ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, FoldState),
    RT = add_param(MP, Param, Ctx),
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% {atom, param, param}
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(MP, _FunState, Ctx, {Op, {param, Param1}, {param, Param2}} =
    _PTree, FoldState)
    when is_atom(Op), element(2, FoldState) == start ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, FoldState),
    NewCtx = add_param(MP, Param1, Ctx),
    RT = add_param(MP, Param2, NewCtx),
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% {atom, param, _}
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(MP, _FunState, Ctx, {Op, {param, Param}, _} = _PTree, FoldState)
    when is_atom(Op), element(2, FoldState) == start ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, FoldState),
    RT = add_param(MP, Param, Ctx),
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% {atom, _, param}
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(MP, _FunState, Ctx, {Op, _, {param, Param}} = _PTree, FoldState)
    when is_atom(Op), element(2, FoldState) == start ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, FoldState),
    RT = add_param(MP, Param, Ctx),
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NO ACTION.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_MP, _FunState, Ctx, _PTree, _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    Ctx.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_param(MP, Param, Ctx) ->
    case MP of
        [] -> [Param | Ctx];
        _ -> case re:run(Param, MP, [global, {capture, [0, 1, 2], binary}]) of
                 {match, Prms} -> Prms ++ Ctx;
                 _ -> Ctx
             end
    end.
