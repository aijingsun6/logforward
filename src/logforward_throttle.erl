-module(logforward_throttle).
-author('alking').
-include("logforward.hrl").
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  report_msg_n/3,
  check_limit/1
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(THROTTLE_ETS, logforward_throttle_ets).
-define(SERVER, ?MODULE).

-record(state, {
  req_queue = [] %{Sink,[From]}
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

report_msg_n(Sink, Throttle, N) ->
  case ets:lookup(?THROTTLE_ETS, Sink) of
    [{_, _, OldN}] when OldN > Throttle andalso N < Throttle -> gen_server:cast(?SERVER, {free_sink, Sink});
    _ -> pass
  end,
  ets:insert(?THROTTLE_ETS, {Sink, Throttle, N}).

%% block until sink is free
check_limit(Sink) ->
  case ets:lookup(?THROTTLE_ETS, Sink) of
    [] -> ok;
    [{_, Throttle, N}] when N < Throttle -> ok;
    [_] ->
      % timeout 60s
      catch gen_server:call(?SERVER, {check_limit, Sink}, 1000 * 60)
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  ets:new(?THROTTLE_ETS, [public, set, named_table, {read_concurrency, true}]),
  {ok, #state{}}.

handle_call({check_limit, Sink}, From, #state{req_queue = PL} = State) ->
  PL2 = case lists:keyfind(Sink, 1, PL) of
          false -> [{Sink, [From]} | PL];
          {_, L} -> lists:keyreplace(Sink, 1, PL, {Sink, [From | L]})
        end,
  {noreply, State#state{req_queue = PL2}};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({free_sink, Sink}, #state{req_queue = PL} = State) ->
  PL2 = case lists:keyfind(Sink, 1, PL) of
          false ->
            [{Sink, []} | PL];
          {_, L} ->
            do_free_sink(L),
            lists:keyreplace(Sink, 1, PL, {Sink, []})
        end,
  {noreply, State#state{req_queue = PL2}};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

do_free_sink([]) -> ok;
do_free_sink([From | L]) ->
  gen_server:reply(From, ok),
  do_free_sink(L).

%%%===================================================================
%%% Internal functions
%%%===================================================================
