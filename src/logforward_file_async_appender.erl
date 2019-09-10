-module(logforward_file_async_appender).
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

msg(_Msg, State) ->
  {ok, State}.