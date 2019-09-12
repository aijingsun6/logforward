-module(logforward_transform).
-author('alking').

-export([parse_transform/2]).

parse_transform(AST, _Opt) ->
  walk_ast([], AST).

walk_ast(Acc, []) ->
  lists:reverse(Acc);
walk_ast(Acc, [{attribute, _, module, {Module, _}} = H | T]) ->
  %% A wild parameterized module appears!
  put(module, Module),
  walk_ast([H | Acc], T);
walk_ast(Acc, [{attribute, _, module, Module} = H | T]) ->
  put(module, Module),
  walk_ast([H | Acc], T);
walk_ast(Acc, [{function, Line, Name, Arity, Clauses} | T]) ->
  put(function, Name),
  put(function_arity, Arity),
  walk_ast([{function, Line, Name, Arity, walk_clauses([], Clauses)} | Acc], T);
walk_ast(Acc, [H | T]) ->
  walk_ast([H | Acc], T).

walk_clauses(Acc, []) ->
  lists:reverse(Acc);
walk_clauses(Acc, [{clause, Line, Arguments, Guards, Body} | T]) ->
  walk_clauses([{clause, Line, Arguments, Guards, walk_body([], Body)} | Acc], T).

walk_body(Acc, []) ->
  lists:reverse(Acc);
walk_body(Acc, [H | T]) ->
  Stmt = transform_statement(H),
  walk_body([Stmt | Acc], T).

transform_statement({call, Line, {remote, _Line1, {atom, _Line2, Module}, {atom, _Line3, Function}}, Args}) ->
  Args2 = transform_args(get(module), Line, Module, Function, Args),
  {call, Line, {remote, _Line1, {atom, _Line2, Module}, {atom, _Line3, Function}}, Args2};
transform_statement(Stmt) when is_tuple(Stmt) ->
  list_to_tuple(transform_statement(tuple_to_list(Stmt)));
transform_statement(Stmt) when is_list(Stmt) ->
  [transform_statement(S) || S <- Stmt];
transform_statement(Stmt) ->
  Stmt.

transform_args(Module, Line, Mod, Func, Args) when Mod == logforward ->
  case lists:member(Func, [debug, info, warn, error, fatal]) of
    true ->
      L = [
        {tuple, Line, [{atom, Line, module}, {atom, Line, Module}]},
        {tuple, Line, [{atom, Line, line}, {integer, Line, Line}]},
        {tuple, Line, [{atom, Line, function}, {atom, Line, get(function)}]},
        {tuple, Line, [{atom, Line, function_arity}, {integer, Line, get(function_arity)}]}
      ],
      case Args of
        [F, A] -> [F, A, join_metadata({nil, Line}, Line, L)];
        [F, A, M] -> [F, A, join_metadata(M, Line, L)];
        [S, F, A, M] -> [S, F, A, join_metadata(M, Line, L)];
        _ -> Args
      end;
    false ->
      Args
  end;
transform_args(_Module, _Line, _Mod, _Func, Args) ->
  Args.

join_metadata(Acc, _Line, []) -> Acc;
join_metadata(Acc, Line, [E | L]) ->
  Acc2 = {cons, Line, E, Acc},
  join_metadata(Acc2, Line, L).
