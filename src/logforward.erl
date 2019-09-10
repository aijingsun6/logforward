-module(logforward).
-include("logforward.hrl").
%% API
-export([
  start/0
]).
-export([
  info/2
]).

start() ->
  application:start(?MODULE).

info(Format, Args) ->
  log(?LOG_LEVEL_INFO, ?DEFAULT_SINK, Format, Args, []).

log(Level, Sink, Format, Args, MetaData) ->
  gen_event:notify(Sink, #logforward_msg{level = Level, metadata = MetaData, format = Format, args = Args}).