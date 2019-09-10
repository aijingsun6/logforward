-module(logforward_app).
-include("logforward.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  R = logforward_sup:start_link(),
  install_sinks(application:get_env(logforward, sinks, [])),
  R.

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
install_sinks([]) -> ok;
install_sinks([{SinkName, Dir, CutLevel, Appends} | L]) ->
  logforward_sink_sup:start_child(SinkName),
  logforward_util:set({SinkName, ?CONFIG_CUT_LEVEL}, CutLevel),
  add_appender(SinkName, Dir, CutLevel, Appends),
  install_sinks(L).

add_appender(_SinkName, _Dir, _CutLevel, []) -> ok;
add_appender(SinkName, Dir, CutLevel, [{Mod, Opt} | L]) ->
  logforward_sink:add_handler(SinkName, Mod, [{?CONFIG_LOG_DIR, Dir}, {?CONFIG_CUT_LEVEL, CutLevel} | Opt]),
  add_appender(SinkName, Dir, CutLevel, L).

