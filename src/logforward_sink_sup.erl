-module(logforward_sink_sup).
-author('alking').

-behaviour(supervisor).

%% API
-export([
  start_link/0,
  add_sink/3,
  remove_sink/1
]).

%% Supervisor callbacks
-export([
  init/1
]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

add_sink(SinkName, SinkOpt, Appends) ->
  ChildSpec = {SinkName, {logforward_sink, start_link, [SinkName, SinkOpt, Appends]}, permanent, 5000, worker, [logforward_sink]},
  supervisor:start_child(?SERVER, ChildSpec).

remove_sink(SinkName)->
  supervisor:terminate_child(?SERVER,SinkName),
  supervisor:delete_child(?SERVER,SinkName).

init([]) ->
  L = application:get_env(logforward, sinks, []),
  Children = parse_sinks(L, []),
  {ok, {{one_for_one, 10, 60}, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_sinks([], Acc) -> Acc;
parse_sinks([{SinkName, SinkOpt, Appends} | L], Acc) ->
  E = {SinkName, {logforward_sink, start_link, [SinkName, SinkOpt, Appends]}, permanent, 5000, worker, [logforward_sink]},
  parse_sinks(L, [E | Acc]).