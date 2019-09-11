-module(logforward_console_appender).
-author('alking').
-include("logforward.hrl").
-behaviour(logforward_appender).

-export([
  init/3,
  handle_msg/3,
  terminate/2
]).

-export([start/1, loop/1]).

-record(state, {
  sink,
  name,
  formatter,
  formatter_config,
  io_pid
}).

init(Sink, Name, Options) ->
  Formatter = proplists:get_value(?APPENDER_CONF_FORMATTER, Options, ?APPENDER_FORMATTER_DEFAULT),
  FormatterConf = case lists:keyfind(?APPENDER_FORMAT_PATTERN, 1, Options) of
                    {_, Str} -> Formatter:parse_pattern(Str);
                    false -> ?APPENDER_FORMATTER_CONFIG_DEFAULT
                  end,
  Pid = start(self()),
  {ok, #state{sink = Sink, name = Name, formatter = Formatter, formatter_config = FormatterConf, io_pid = Pid}}.

handle_msg(Msg, Extra, State) ->
  catch do_log(Msg, Extra, State),
  {ok, State}.

terminate(_Reason, #state{io_pid = Pid}) when is_pid(Pid) ->
  Pid ! close,
  ok;
terminate(_Reason, _State) ->
  ok.

do_log(Msg, Extra, #state{formatter = Formatter, formatter_config = FormatterConf, io_pid = Pid}) ->
  case logforward_util:format_msg_with_cache(Msg, Formatter, FormatterConf, Extra) of
    Str when erlang:is_list(Str) -> Pid ! {append, Str};
    _ -> pass
  end.

start(Parent) ->
  erlang:spawn(fun() -> start_i(Parent) end).

start_i(Parent) ->
  Ref = erlang:monitor(process, Parent),
  loop(Ref).

loop(MonitorRef) ->
  receive
    {append, Str} ->
      catch io:put_chars(Str),
      ?MODULE:loop(MonitorRef);
    {append, IO, Str} ->
      catch io:put_chars(IO, Str),
      ?MODULE:loop(MonitorRef);
    {'DOWN', MonitorRef, _, _, _} ->
      erlang:demonitor(MonitorRef), ok;
    close ->
      erlang:demonitor(MonitorRef), ok;
    _ ->
      ?MODULE:loop(MonitorRef)
  end.