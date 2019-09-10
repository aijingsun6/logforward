-module(logforward_default_formatter_test).
-author('alking').

-include_lib("eunit/include/eunit.hrl").

parse_pattern_test() ->
  ?assertEqual([], logforward_default_formatter:parse_pattern("")),
  ?assertEqual([module], logforward_default_formatter:parse_pattern("%module")),
  ?assertEqual([datetime, " [", level, "] ", msg, eol], logforward_default_formatter:parse_pattern("%datetime [%level] %msg%eol")),
  ?assertEqual([datetime, " [", level, "]-", msg, eol], logforward_default_formatter:parse_pattern("%datetime [%level]-%msg%eol")),
  ?assertEqual([" ",datetime, " [", level, "]-", msg, eol], logforward_default_formatter:parse_pattern(" %datetime [%level]-%msg%eol")).