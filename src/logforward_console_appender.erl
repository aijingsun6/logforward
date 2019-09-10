-module(logforward_console_appender).
-author('alking').
-include("logforward.hrl").
-behaviour(logforward_appender).

-export([
  init/3,
  handle_msg/2,
  terminate/2
]).

-record(state, {
  sink,
  name,
  formatter,
  formatter_config,
  nmsg
}).

init(Sink, Name, Options) ->
  Formatter = proplists:get_value(?APPENDER_CONF_FORMATTER, Options, ?APPENDER_FORMATTER_DEFAULT),
  FormatterConf = case lists:keyfind(?APPENDER_FORMAT_PATTERN, 1, Options) of
                    {_, Str} -> Formatter:parse_pattern(Str);
                    false -> ?APPENDER_FORMATTER_CONFIG_DEFAULT
                  end,
  {ok, #state{sink = Sink, name = Name, formatter = Formatter, formatter_config = FormatterConf, nmsg = 0}}.

handle_msg(Msg, #state{formatter = Formatter, formatter_config = FormatterConf, nmsg = N} = State) ->
  N2 = N + 1,
  Extra = [{nmsg, N2}],
  catch do_log(Msg, Formatter, FormatterConf, Extra),
  {ok, State#state{nmsg = N2}}.

terminate(_Reason, _State) ->
  ok.

do_log(Msg, Formatter, FormatterConf, Extra) ->
  case Formatter:format(Msg, FormatterConf, Extra) of
    L when erlang:is_list(L) -> io:put_chars(L);
    _ -> pass
  end.