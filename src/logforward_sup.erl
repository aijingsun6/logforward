-module(logforward_sup).
-include("logforward.hrl").
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([
  init/1
]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  Children = [
    {logforward_throttle, {logforward_throttle, start_link, []}, permanent, 5000, worker, [logforward_throttle]},
    {logforward_sink_sup, {logforward_sink_sup, start_link, []}, permanent, 5000, supervisor, [logforward_sink_sup]}
  ],
  {ok, {{one_for_one, 10, 60}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
