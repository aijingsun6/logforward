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
  try persistent_term:get({?TBL, Key}) of
    Value -> Value
  catch
    _ ->
      Default
  end.

%% config set
set(Key, Value) ->
  persistent_term:put({?TBL, Key}, Value).
