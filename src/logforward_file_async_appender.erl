-module(logforward_file_async_appender).
-author('alking').

-behaviour(logforward_appender).

-export([
  init/1,
  handle_msg/2,
  terminate/2
]).

-record(state, {
  options
}).

init(Options) ->
  {ok, #state{options = Options}}.

handle_msg(_Msg, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.