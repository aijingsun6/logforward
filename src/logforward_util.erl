-module(logforward_util).
-author('alking').
-include("logforward.hrl").

-define(TBL, logforward_config).
%% API
-export([
  get/2,
  set/2
]).

%% config get
get(Key, Default) ->
  case catch persistent_term:get({?TBL, Key}) of
    {'EXIT', _} -> Default;
    Value -> Value
  end.

%% config set
set(Key, Value) ->
  persistent_term:put({?TBL, Key}, Value).
