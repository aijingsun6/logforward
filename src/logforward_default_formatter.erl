-module(logforward_default_formatter).
-author('alking').
-behaviour(logforward_formatter).

%% API
-export([parse_pattern/1, format/2]).

parse_pattern(_Pattern) ->
  erlang:error(not_implemented).

format(_Msg, _Config) ->
  erlang:error(not_implemented).