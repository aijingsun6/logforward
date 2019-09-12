-module(logforward_throttle).
-author('alking').
-include("logforward.hrl").
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  report_msg_n/3,
  check_limit/1,
  free_sink/1,
  free_sink/2
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

%% {Sink,Throttle,N}
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

%% 上报消息队列长度
report_msg_n(Sink, Throttle, N) ->
  case ets:lookup(?THROTTLE_ETS, Sink) of
    [{_, OldThrottle, OldN}] when OldN > OldThrottle andalso N < Throttle ->
      % free a  block sink
      FreeN = Throttle - N,
      gen_server:cast(?SERVER, {free_sink, Sink, FreeN});
    _ ->
      pass
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

free_sink(Sink) -> gen_server:cast(?SERVER, {free_sink, Sink}).

free_sink(Sink, N) -> gen_server:cast(?SERVER, {free_sink, Sink, N}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  ets:new(?THROTTLE_ETS, [public, set, named_table, {read_concurrency, true}]),
  {ok, #state{}}.

handle_call({check_limit, Sink}, From, State) ->
  handle_check_limit(Sink, From, State);
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({free_sink, Sink, FreeN}, State) ->
  State2 = do_free_sink(Sink, FreeN, State),
  {noreply, State2};
handle_cast({free_sink, Sink}, State) ->
  State2 = do_free_sink(Sink, State),
  {noreply, State2};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_free_sink(Sink, #state{req_queue = PL} = State) ->
  PL2 = case lists:keyfind(Sink, 1, PL) of
          false ->
            [{Sink, []} | PL];
          {_, L} ->
            lists:foreach(fun(X) -> gen_server:reply(X, ok) end, L),
            lists:keyreplace(Sink, 1, PL, {Sink, []})
        end,
  State#state{req_queue = PL2}.

do_free_sink(Sink, N, #state{req_queue = PL} = State) ->
  PL2 = case lists:keyfind(Sink, 1, PL) of
          false ->
            [{Sink, []} | PL];
          {_, L} ->
            L2 = do_free_sink_i(L, N, 0),
            lists:keyreplace(Sink, 1, PL, {Sink, L2})
        end,
  State#state{req_queue = PL2}.

do_free_sink_i([], _N, _Acc) ->
  [];
do_free_sink_i(L, N, Acc) when Acc >= N ->
  L;
do_free_sink_i([From | L], N, Acc) ->
  gen_server:reply(From, ok),
  do_free_sink_i(L, N, Acc + 1).

handle_check_limit(Sink, From, State) ->
  case ets:lookup(?THROTTLE_ETS, Sink) of
    [{_, Throttle, N}] when N > Throttle ->
      % block sink
      block_sink(Sink, From, State);
    _ ->
      State2 = do_free_sink(Sink, State),
      {reply, ok, State2}
  end.

block_sink(Sink, From, #state{req_queue = PL} = State) ->
  PL2 = case lists:keyfind(Sink, 1, PL) of
          false ->
            [{Sink, [From]} | PL];
          {_, L} ->
            L2 = lists:reverse([From | L]),
            lists:keyreplace(Sink, 1, PL, {Sink, L2})
        end,
  {noreply, State#state{req_queue = PL2}}.