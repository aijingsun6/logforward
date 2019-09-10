-module(logforward_app).
-include("logforward.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  logforward_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.
