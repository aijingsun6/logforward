-module(logforward_sink_sup).
-author('alking').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([
  init/1,
  start_child/1
]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Name) when is_atom(Name) ->
  supervisor:start_child(?MODULE, [Name]).

init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxR = 0,
  MaxT = 3600,
  Name = undefined, % As simple_one_for_one is used.
  StartFunc = {logforward_sink, start_link, []},
  Restart = permanent, % E.g. should not be restarted
  Shutdown = 4000,
  Modules = [logforward_sink],
  Type = worker,
  ChildSpec = {Name, StartFunc, Restart, Shutdown, Type, Modules},
  {ok, {{RestartStrategy, MaxR, MaxT}, [ChildSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
