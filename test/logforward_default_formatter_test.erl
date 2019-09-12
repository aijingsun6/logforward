-module(logforward_default_formatter_test).
-author('alking').
-include("logforward.hrl").
-include_lib("eunit/include/eunit.hrl").

parse_pattern_test() ->
  ?assertEqual([], logforward_default_formatter:parse_pattern("")),
  ?assertEqual([module], logforward_default_formatter:parse_pattern("%module")),
  ?assertEqual(["HEAD",module], logforward_default_formatter:parse_pattern("HEAD%module")),
  ?assertEqual([datetime, " [", level, "] ", msg, eol], logforward_default_formatter:parse_pattern("%datetime [%level] %msg%eol")),
  ?assertEqual([datetime, " [", level, "]-", msg, eol], logforward_default_formatter:parse_pattern("%datetime [%level]-%msg%eol")),
  ?assertEqual([" ", datetime, " [", level, "]-", msg, eol], logforward_default_formatter:parse_pattern(" %datetime [%level]-%msg%eol")).

format_test() ->
  ?assertEqual("2019-09-10 12:01:02.234 [DEBUG] - abcde\r\n",
    logforward_default_formatter:format(#logforward_msg{datetime = {{2019, 9, 10}, {12, 1, 2}}, timestamp_ms = 1234, level = debug, format = "~p", args = [abcde]},
      [datetime, " [", level, "] - ", msg, eol], [])),

  ?assertEqual("2019-09-10 12:01:02.234 [DEBUG] - 111-abcde-222\r\n",
    logforward_default_formatter:format(#logforward_msg{datetime = {{2019, 9, 10}, {12, 1, 2}}, timestamp_ms = 1234, level = debug, format = "111-~p-222", args = [abcde]},
      [datetime, " [", level, "] - ", msg, eol], [])),
  % with metadata
  ?assertEqual("2019-09-10 12:01:02.234 [k1=v1&k2=v2] - 111-abcde-222\r\n",
    logforward_default_formatter:format(#logforward_msg{datetime = {{2019, 9, 10}, {12, 1, 2}}, timestamp_ms = 1234, level = debug, format = "111-~p-222", args = [abcde],
      metadata = [ {k1,v1},{k2,v2},{k,v,e} ]},
      [datetime, " [",metadata, "] - ", msg, eol], [])),

  ok.

format_file_test() ->
  ?assertEqual("info.0.log", logforward_default_formatter:format_file(["info.", nth, ".log"], [{nth, 0}])),
  ?assertEqual("info.2019-09-11.0.log", logforward_default_formatter:format_file(["info.", date, ".", nth, ".log"], [{nth, 0}, {date, {2019, 9, 11}}])),
  ok.