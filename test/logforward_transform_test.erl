-module(logforward_transform_test).
-author('alking').

%% API
-export([
  test/0,
  debug_test/0,
  info_test/0,
  warn_test/0,
  error_test/0,
  fatal_test/0
]).

test() ->
  debug_test(),
  info_test(),
  warn_test(),
  error_test(),
  fatal_test().

debug_test() ->
  logforward:debug("debug/2", []),
  logforward:debug("debug/3", [], []),
  logforward:debug(sink, "debug/4", [], []).

info_test() ->
  logforward:info("info/2", []),
  logforward:info("info/3", [], []),
  logforward:info(sink, "info/4", [], []).

warn_test() ->
  logforward:warn("warn/2", []),
  logforward:warn("warn/3", [], []),
  logforward:warn(sink, "warn/4", [], []).

error_test() ->
  logforward:error("error/2", []),
  logforward:error("error/3", [], []),
  logforward:error(sink, "error/4", [], []).


fatal_test() ->
  logforward:fatal("fatal/2", []),
  logforward:fatal("fatal/3", [], []),
  logforward:fatal(sink, "fatal/4", [], []).