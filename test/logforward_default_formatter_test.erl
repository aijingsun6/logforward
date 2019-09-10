-module(logforward_default_formatter_test).
-author('alking').
-include("logforward.hrl").
-include_lib("eunit/include/eunit.hrl").

parse_pattern_test() ->
  ?assertEqual([], logforward_default_formatter:parse_pattern("")),
  ?assertEqual([module], logforward_default_formatter:parse_pattern("%module")),
  ?assertEqual([datetime, " [", level, "] ", msg, eol], logforward_default_formatter:parse_pattern("%datetime [%level] %msg%eol")),
  ?assertEqual([datetime, " [", level, "]-", msg, eol], logforward_default_formatter:parse_pattern("%datetime [%level]-%msg%eol")),
  ?assertEqual([" ", datetime, " [", level, "]-", msg, eol], logforward_default_formatter:parse_pattern(" %datetime [%level]-%msg%eol")),
  ?assertEqual(["file.", nth, ".log"], logforward_default_formatter:parse_pattern("file.%nth.log")).

format_test() ->
  ?assertEqual("2019-09-10 12:01:02.234 [DEBUG] - abcde\r\n", logforward_default_formatter:format(#logforward_msg{datetime = {{2019, 9, 10}, {12, 1, 2}}, timestamp_ms = 1234, level = debug, format = "~p", args = [abcde]}, [datetime, " [", level, "] - ", msg, eol], [])),

  ?assertEqual("2019-09-10 12:01:02.234 [DEBUG] - 111-abcde-222\r\n", logforward_default_formatter:format(#logforward_msg{datetime = {{2019, 9, 10}, {12, 1, 2}}, timestamp_ms = 1234, level = debug, format = "111-~p-222", args = [abcde]}, [datetime, " [", level, "] - ", msg, eol], [])),

  ok.