-module(logforward_default_formatter).
-author('alking').
-behaviour(logforward_formatter).

%% API
-export([parse_pattern/1, format/2]).

parse_pattern(Pattern) -> parse_pattern(Pattern,read_str,[]).

format(_Msg, _Config) ->
  erlang:error(not_implemented).

read_token([C | L], Acc) when C >= $a andalso C =< $z ->
  read_token(L, [C | Acc]);
read_token([C | L], Acc) when C >= $A andalso C =< $Z ->
  read_token(L, [C | Acc]);
read_token(R, Acc) ->
  Low = string:to_lower(lists:reverse(Acc)),
  {erlang:list_to_atom(Low), R}.

read_until([], Acc) ->
  {lists:reverse(Acc), []};
read_until([$\% | L], Acc) ->
  {lists:reverse(Acc), L};
read_until([C | L], Acc) ->
  read_until(L, [C | Acc]).

parse_pattern([], _, Acc) ->
  lists:reverse(Acc);
parse_pattern(Pattern, read_str, Acc) ->
  {H, Left} = read_until(Pattern, []),
  case H == [] of
    true -> parse_pattern(Left, read_token, Acc);
    false -> parse_pattern(Left, read_token, [H | Acc])
  end;
parse_pattern(Pattern, read_token, Acc) ->
  {Token, Left} = read_token(Pattern, []),
  parse_pattern(Left, read_str, [Token | Acc]).


