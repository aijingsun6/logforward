-module(logforward_default_formatter).
-author('alking').
-include("logforward.hrl").
-behaviour(logforward_formatter).

%% API
-export([parse_pattern/1, format/3, format_file/2]).

parse_pattern(Pattern) -> parse_pattern(Pattern, read_str, []).

format(Msg, Config, Extra) -> format(Config, Msg, Extra, []).

format_file(Config, Options) -> format_file(Config, Options, []).


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

format([], _Msg, _Extra, Acc) ->
  lists:flatten(lists:reverse(Acc));
format([module | L], #logforward_msg{module = Module} = Msg, Extra, Acc) ->
  format(L, Msg, Extra, [logforward_util:to_string(Module) | Acc]);
format([date | L], #logforward_msg{datetime = {Day, _}} = Msg, Extra, Acc) ->
  format(L, Msg, Extra, [logforward_util:date_to_string(Day) | Acc]);
format([time | L], #logforward_msg{datetime = {_, Time}, timestamp_ms = TS} = Msg, Extra, Acc) ->
  format(L, Msg, Extra, [logforward_util:time_to_string(Time, TS rem 1000) | Acc]);
format([datetime | L], #logforward_msg{datetime = DT, timestamp_ms = TS} = Msg, Extra, Acc) ->
  format(L, Msg, Extra, [logforward_util:datetime_to_string(DT, TS rem 1000) | Acc]);
format([msg | L], #logforward_msg{format = Format, args = Args} = Msg, Extra, Acc) ->
  format(L, Msg, Extra, [lists:flatten(io_lib:format(Format, Args)) | Acc]);
format([eol | L], Msg, Extra, Acc) ->
  format(L, Msg, Extra, ["\n" | Acc]);
format([line | L], #logforward_msg{line = Line} = Msg, Extra, Acc) ->
  format(L, Msg, Extra, [logforward_util:to_string(Line) | Acc]);
format([level | L], #logforward_msg{level = Level} = Msg, Extra, Acc) ->
  format(L, Msg, Extra, [logforward_util:level_str(Level) | Acc]);
format([pid | L], #logforward_msg{pid = Pid} = Msg, Extra, Acc) ->
  format(L, Msg, Extra, [logforward_util:to_string(Pid) | Acc]);
format([node | L], #logforward_msg{node = Node} = Msg, Extra, Acc) ->
  format(L, Msg, Extra, [logforward_util:to_string(Node) | Acc]);
format([nmsg | L], Msg, Extra, Acc) ->
  N = proplists:get_value(nmsg, Extra, 0),
  format(L, Msg, Extra, [logforward_util:to_string(N) | Acc]);
format([function | L], #logforward_msg{function = Func} = Msg, Extra, Acc) ->
  format(L, Msg, Extra, [logforward_util:to_string(Func) | Acc]);
format([function_arity | L], #logforward_msg{function_arity = FA} = Msg, Extra, Acc) ->
  format(L, Msg, Extra, [logforward_util:to_string(FA) | Acc]);
format([nth | L], Msg, Extra, Acc) ->
  N = proplists:get_value(nth, Extra, 0),
  format(L, Msg, Extra, [logforward_util:to_string(N) | Acc]);
format([metadata | L], #logforward_msg{metadata = Meta} = Msg, Extra, Acc) ->
  Str = metadata_to_string(Meta),
  format(L, Msg, Extra, [Str | Acc]);
format([E | L], Msg, Extra, Acc) when is_list(E) ->
  format(L, Msg, Extra, [E | Acc]);
format([_ | L], Msg, Extra, Acc) ->
  format(L, Msg, Extra, Acc).

format_file([], _, Acc) ->
  lists:flatten(lists:reverse(Acc));
format_file([nth | L], Options, Acc) ->
  N = proplists:get_value(nth, Options, 0),
  format_file(L, Options, [logforward_util:to_string(N) | Acc]);
format_file([date | L], Options, Acc) ->
  {Day, _} = calendar:local_time(),
  V = proplists:get_value(date, Options, Day),
  format_file(L, Options, [logforward_util:date_to_string(V) | Acc]);
format_file([E | L], Options, Acc) when is_list(E) ->
  format_file(L, Options, [E | Acc]);
format_file([_ | L], Options, Acc) ->
  format_file(L, Options, Acc).

metadata_to_string(L) ->
  L2 = lists:filtermap(
    fun({K, V}) -> {true, [logforward_util:to_string(K), "=", logforward_util:to_string(V)]};
      (_) -> false
    end,
    L
  ),
  lists:flatten(lists:join("&", L2)).

