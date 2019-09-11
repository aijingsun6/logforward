-module(logforward_util_test).
-author('alking').
-include("logforward.hrl").
-include_lib("eunit/include/eunit.hrl").
%% API
-export([]).
-define(FORMAT_CACHE_KEY, format_cache).
format_msg_with_cache_test() ->
  Msg = #logforward_msg{datetime = {{2019, 9, 10}, {12, 1, 2}}, timestamp_ms = 1234, level = debug, format = "~p", args = [abcde]},
  Formatter = logforward_default_formatter,
  NMsg = 1,
  Extra = [{nmsg, NMsg}],
  FormatterConf = ?APPENDER_FORMATTER_CONFIG_DEFAULT,
  FormatRet = logforward_util:format_msg_with_cache(Msg, Formatter, FormatterConf, Extra),
  FormatRetExcept = "2019-09-10 12:01:02.234 [DEBUG] - abcde\r\n",
  ?assertEqual(FormatRetExcept, FormatRet),
  Key = {?FORMAT_CACHE_KEY, {NMsg, Formatter, FormatterConf, Extra}},
  ?assertEqual(FormatRetExcept, erlang:get(Key)),
  logforward_util:clean_format_cache(),
  ?assertEqual(undefined, erlang:get(Key)).