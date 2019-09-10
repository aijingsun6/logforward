-module(logforward_console_appender).
-author('alking').

-behaviour(logforward_appender).

-export([
  init/1,
  msg/2
]).

-record(state, {
  options
}).

init(Options) ->
  {ok, #state{options = Options}}.

msg(Msg, State) ->
  io:format("~p~n", [Msg]),
  {ok, State}.