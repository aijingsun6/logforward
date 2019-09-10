-module(logforward).
-include("logforward.hrl").
%% API
-export([
  start/0
]).
-export([debug/2, debug/3, debug/4]).
-export([info/2, info/3, info/4]).
-export([warn/2, warn/3, warn/4]).
-export([error/2, error/3, error/4]).
-export([fatal/2, fatal/3, fatal/4]).

start() ->
  application:start(?MODULE).

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
  gen_event:notify(Sink, #logforward_msg{level = Level, metadata = MetaData, format = Format, args = Args}).