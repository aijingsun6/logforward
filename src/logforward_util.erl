-module(logforward_util).
-author('alking').
-include("logforward.hrl").

-define(TBL, logforward_config).
-define(FORMAT_CACHE_KEY, format_cache).
%% API
-export([
  get/2,
  set/2,
  level_str/1,
  to_string/1,
  datetime_to_string/1,
  datetime_to_string/2,
  time_to_string/1,
  time_to_string/2,
  date_to_string/1
]).

-export([
  format_msg_with_cache/4,
  clean_format_cache/0
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

level_str(?LOG_LEVEL_DEBUG) -> "DEBUG";
level_str(?LOG_LEVEL_INFO) -> "INFO";
level_str(?LOG_LEVEL_WARN) -> "WARN";
level_str(?LOG_LEVEL_ERROR) -> "ERROR";
level_str(?LOG_LEVEL_FATAL) -> "FATAL";
level_str(?LOG_LEVEL_NONE) -> "NONE".

to_string(A) when is_atom(A) -> erlang:atom_to_list(A);
to_string(L) when is_list(L) -> L;
to_string(B) when is_binary(B) -> erlang:binary_to_list(B);
to_string(I) when is_integer(I) -> erlang:integer_to_list(I);
to_string(A) -> lists:flatten(io_lib:format("~p", [A])).

datetime_to_string({{Y, M, D}, {H, MM, S}}) ->
  lists:flatten(io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Y, M, D, H, MM, S])).

datetime_to_string({{Y, M, D}, {H, MM, S}}, MS) ->
  lists:flatten(io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B", [Y, M, D, H, MM, S, MS])).

time_to_string({H, MM, S}) ->
  lists:flatten(io_lib:format("~2.10.0B:~2.10.0B:~2.10.0B", [H, MM, S])).

time_to_string({H, MM, S}, MS) ->
  lists:flatten(io_lib:format("~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B", [H, MM, S, MS])).

date_to_string({Y, M, D}) ->
  lists:flatten(io_lib:format("~B-~2.10.0B-~2.10.0B", [Y, M, D])).

format_msg_with_cache(Msg, Formatter, FormatterConf, Extra) ->
  NMsg = proplists:get_value(nmsg, Extra, 0),
  Key = {?FORMAT_CACHE_KEY, {NMsg, Formatter, FormatterConf, Extra}},
  case erlang:get(Key) of
    V when is_list(V) ->
      V;
    _ ->
      case catch Formatter:format(Msg, FormatterConf, Extra) of
        Ret when erlang:is_list(Ret) ->
          erlang:put(Key, Ret),
          Ret;
        _ ->
          undefined
      end
  end.

clean_format_cache() ->
  L = erlang:get(),
  clean_format_cache(L).

clean_format_cache([]) -> ok;
clean_format_cache([{{?FORMAT_CACHE_KEY, _} = Key, _} | L]) ->
  erlang:erase(Key),
  clean_format_cache(L);
clean_format_cache([_ | L]) ->
  clean_format_cache(L).