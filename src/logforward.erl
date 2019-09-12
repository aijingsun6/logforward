-module(logforward).
-include("logforward.hrl").
%% API
-export([
  start/0,
  stop/0,
  set_cut_level/1,
  set_cut_level/2,
  set_appender_level/2,
  set_appender_level/3,
  add_sink/3,
  remove_sink/1
]).


-export([debug/2, debug/3, debug/4]).
-export([info/2, info/3, info/4]).
-export([warn/2, warn/3, warn/4]).
-export([error/2, error/3, error/4]).
-export([fatal/2, fatal/3, fatal/4]).

start() ->
  application:start(?MODULE).

stop() ->
  application:stop(?MODULE).

set_cut_level(Level) -> logforward_sink:set_cut_level(?DEFAULT_SINK, Level).
set_cut_level(Sink, Level) -> logforward_sink:set_cut_level(Sink, Level).

set_appender_level(Appender, Level) -> logforward_sink:set_appender_level(?DEFAULT_SINK, Appender, Level).
set_appender_level(Sink, Appender, Level) -> logforward_sink:set_appender_level(Sink, Appender, Level).

add_sink(SinkName, SinkOpt, Appends) -> logforward_sink_sup:add_sink(SinkName, SinkOpt, Appends).

remove_sink(SinkName) -> logforward_sink_sup:remove_sink(SinkName).

debug(Format, Args) ->
  log(?DEFAULT_SINK, ?LOG_LEVEL_DEBUG, Format, Args, []).

debug(Format, Args, MetaData) ->
  log(?DEFAULT_SINK, ?LOG_LEVEL_DEBUG, Format, Args, MetaData).

debug(Sink, Format, Args, MetaData) when is_atom(Sink) ->
  log(Sink, ?LOG_LEVEL_DEBUG, Format, Args, MetaData).

info(Format, Args) ->
  log(?DEFAULT_SINK, ?LOG_LEVEL_INFO, Format, Args, []).

info(Format, Args, MetaData) ->
  log(?DEFAULT_SINK, ?LOG_LEVEL_INFO, Format, Args, MetaData).

info(Sink, Format, Args, MetaData) when is_atom(Sink) ->
  log(Sink, ?LOG_LEVEL_INFO, Format, Args, MetaData).

warn(Format, Args) ->
  log(?DEFAULT_SINK, ?LOG_LEVEL_WARN, Format, Args, []).

warn(Format, Args, MetaData) ->
  log(?DEFAULT_SINK, ?LOG_LEVEL_WARN, Format, Args, MetaData).

warn(Sink, Format, Args, MetaData) when is_atom(Sink) ->
  log(Sink, ?LOG_LEVEL_WARN, Format, Args, MetaData).

error(Format, Args) ->
  log(?DEFAULT_SINK, ?LOG_LEVEL_ERROR, Format, Args, []).

error(Format, Args, MetaData) ->
  log(?DEFAULT_SINK, ?LOG_LEVEL_ERROR, Format, Args, MetaData).

error(Sink, Format, Args, MetaData) when is_atom(Sink) ->
  log(Sink, ?LOG_LEVEL_ERROR, Format, Args, MetaData).

fatal(Format, Args) ->
  log(?DEFAULT_SINK, ?LOG_LEVEL_FATAL, Format, Args, []).

fatal(Format, Args, MetaData) ->
  log(?DEFAULT_SINK, ?LOG_LEVEL_FATAL, Format, Args, MetaData).

fatal(Sink, Format, Args, MetaData) when is_atom(Sink) ->
  log(Sink, ?LOG_LEVEL_FATAL, Format, Args, MetaData).

log(Sink, Level, Format, Args, MetaData) ->
  Msg = #logforward_msg{level = Level, format = Format, args = Args},
  Msg2 = parse_msg([module, line, pid, node, function, function_arity], MetaData, Msg),
  Msg3 = Msg2#logforward_msg{datetime = calendar:local_time(), timestamp_ms = timestamp_ms()},
  logforward_sink:msg(Sink, Msg3).

parse_del_opt(Key, Options, Def) ->
  case lists:keyfind(Key, 1, Options) of
    {_, V} -> {V, lists:keydelete(Key, 1, Options)};
    false -> {Def, Options}
  end.

proc_name() ->
  case process_info(self(), registered_name) of
    {registered_name, Name} ->
      Name;
    [] ->
      self()
  end.
parse_msg([], MetaData, Acc) -> Acc#logforward_msg{metadata = MetaData};
parse_msg([module | L], MetaData, Acc) ->
  {V, MetaData2} = parse_del_opt(module, MetaData, none),
  parse_msg(L, MetaData2, Acc#logforward_msg{module = V});
parse_msg([line | L], MetaData, Acc) ->
  {V, MetaData2} = parse_del_opt(line, MetaData, 0),
  parse_msg(L, MetaData2, Acc#logforward_msg{line = V});
parse_msg([pid | L], MetaData, Acc) ->
  {V, MetaData2} = parse_del_opt(pid, MetaData, proc_name()),
  parse_msg(L, MetaData2, Acc#logforward_msg{pid = V});
parse_msg([node | L], MetaData, Acc) ->
  {V, MetaData2} = parse_del_opt(node, MetaData, node()),
  parse_msg(L, MetaData2, Acc#logforward_msg{node = V});
parse_msg([function | L], MetaData, Acc) ->
  {V, MetaData2} = parse_del_opt(function, MetaData, none),
  parse_msg(L, MetaData2, Acc#logforward_msg{function = V});
parse_msg([function_arity | L], MetaData, Acc) ->
  {V, MetaData2} = parse_del_opt(function_arity, MetaData, 0),
  parse_msg(L, MetaData2, Acc#logforward_msg{function_arity = V});
parse_msg(_, MetaData, Acc) ->
  Acc#logforward_msg{metadata = MetaData}.

timestamp_ms() ->
  {Meg, Sec, Micro} = os:timestamp(),
  Meg * 1000 * 1000 * 1000 + Sec * 1000 + Micro div 1000.



