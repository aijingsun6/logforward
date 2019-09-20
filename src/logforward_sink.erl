-module(logforward_sink).
-author('alking').
-include("logforward.hrl").
-behaviour(gen_server).

%% API
-export([
  start_link/3,
  set_cut_level/2,
  msg/2
]).

-export([
  appenders/1,
  add_appender/2,
  remove_appender/2,
  set_appender_level/3
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

-record(appender, {
  name,
  mod,
  options,
  level,
  state
}).

-record(state, {
  sink,
  options,
  cut_level,
  throttle,
  garbage_msg,
  report_msg,
  nmsg,
  appender = [] % {Name,#appender{}}
}).

start_link(Name, Options, Appends) when is_atom(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name, Options, Appends], []).

msg(Name, #logforward_msg{level = Level} = Msg) ->
  CutLevel = logforward_util:get({Name, ?CONFIG_CUT_LEVEL}, ?SINK_CUT_LEVEL_DEFAULT),
  case ?LEVEL2INT(Level) >= ?LEVEL2INT(CutLevel) of
    true ->
      case erlang:whereis(Name) of
        PID when is_pid(PID) ->
          logforward_throttle:check_limit(Name),
          gen_server:cast(Name, {msg, Msg});
        _ ->
          {error, no_sink}
      end;
    false ->
      pass
  end.

set_cut_level(Sink, Level) ->
  case lists:member(Level, ?LOG_LEVEL_ALL) of
    true ->
      gen_server:call(Sink, {set_cut_level, Level}, 5000);
    false ->
      {fail, level_error}
  end.

set_appender_level(Sink, Appender, Level) ->
  case lists:member(Level, ?LOG_LEVEL_ALL) of
    true ->
      gen_server:call(Sink, {set_appender_level, Appender, Level}, 5000);
    false ->
      {fail, level_error}
  end.

appenders(Sink) when is_atom(Sink) ->
  gen_server:call(Sink, {appenders}, 5000).

add_appender(Sink, {Name, Mod, Opt} = E) when is_atom(Sink), is_atom(Name), is_atom(Mod), is_list(Opt) ->
  gen_server:call(Sink, {add_appender, E}, 5000).

remove_appender(Sink, Name) when is_atom(Sink), is_atom(Name) ->
  gen_server:call(Sink, {remove_appender, Name}, 5000).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name, Options, Appends]) ->
  L = install_appender(Appends, Name, Options, []),
  CutLevel = proplists:get_value(?CONFIG_CUT_LEVEL, Options, ?SINK_CUT_LEVEL_DEFAULT),
  Throttle = proplists:get_value(?CONFIG_THROTTLE, Options, ?SINK_THROTTLE_DEFAULT),
  logforward_util:set({Name, ?CONFIG_CUT_LEVEL}, CutLevel),
  logforward_util:set({Name, ?CONFIG_THROTTLE}, Throttle),
  Garbage = proplists:get_value(?CONFIG_GARBAGE_MSG, Options, ?SINK_GARBAGE_DEFAULT),
  Report = proplists:get_value(?CONFIG_REPORT_MSG, Options, ?SINK_REPORT_MSG_DEFAULT),
  true = Report < Throttle,
  {ok, #state{sink = Name,
    options = Options,
    cut_level = CutLevel,
    throttle = Throttle,
    garbage_msg = Garbage,
    report_msg = Report,
    appender = L,
    nmsg = 0
  }}.

handle_call({appenders}, _From, #state{appender = L} = State) ->
  L2 = lists:map(fun({X, _}) -> X end, L),
  {reply, L2, State};

handle_call({add_appender, {Name, _Mod, _Opt} = E}, _From, #state{sink = Sink, options = Opt, appender = L} = State) ->
  case lists:keyfind(Name, 1, L) of
    {_, _} ->
      {reply, appende_exists, State};
    false ->
      L2 = install_appender([E], Sink, Opt, L),
      {reply, ok, State#state{appender = L2}}
  end;

handle_call({remove_appender, Name}, _From, #state{appender = L} = State) ->
  case lists:keyfind(Name, 1, L) of
    {_, _} = E ->
      terminate_appender([E], normal),
      L2 = lists:keydelete(Name, 1, L),
      {reply, ok, State#state{appender = L2}};
    false ->
      {reply, appende_not_found, State}
  end;

handle_call({set_cut_level, Level}, _From, #state{sink = Name, options = Opts} = State) ->
  Opts2 = case lists:keyfind(?CONFIG_CUT_LEVEL, 1, Opts) of
            false -> [{?CONFIG_CUT_LEVEL, Level} | Opts];
            {_, _} -> lists:keyreplace(?CONFIG_CUT_LEVEL, 1, Opts, {?CONFIG_CUT_LEVEL, Level})
          end,
  logforward_util:set({Name, ?CONFIG_CUT_LEVEL}, Level),
  {reply, ok, State#state{cut_level = Level, options = Opts2}};

handle_call({set_appender_level, Appender, Level}, _From, #state{appender = L} = State) ->
  case lists:keyfind(Appender, 1, L) of
    {_, #appender{} = E} ->
      E2 = E#appender{level = Level},
      L2 = lists:keyreplace(Appender, 1, L, {Appender, E2}),
      {reply, ok, State#state{appender = L2}};
    false ->
      {reply, appender_not_found, State}
  end;

handle_call({msg, Msg}, _From, State) ->
  State2 = do_deal_msg(Msg, State),
  catch gc(State2),
  catch report_msg_n(State2),
  {reply, ok, State2};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({msg, Msg}, State) ->
  State2 = do_deal_msg(Msg, State),
  {noreply, State2};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, #state{appender = L}) ->
  terminate_appender(L, Reason),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
merge_appender_option([], Acc) -> Acc;
merge_appender_option([{K, V} | L], Acc) ->
  case lists:keyfind(K, 1, Acc) of
    {_, _} -> merge_appender_option(L, Acc);
    false -> merge_appender_option(L, [{K, V} | Acc])
  end.

install_appender([], _SinkName, _SinkOpt, Acc) -> Acc;
install_appender([{Name, Mod, Opt} | L], SinkName, SinkOpt, Acc) ->
  Opt2 = merge_appender_option(SinkOpt, Opt),
  Level = proplists:get_value(?CONFIG_LEVEL, Opt2, ?APPENDER_LEVEL_DEFAULT),
  {ok, State} = Mod:init(SinkName, Name, Opt2),
  Appender = #appender{name = Name, mod = Mod, options = Opt2, level = Level, state = State},
  install_appender(L, SinkName, SinkOpt, [{Name, Appender} | Acc]).

do_deal_msg(Msg, #state{appender = L, nmsg = N} = State) ->
  N2 = N + 1,
  Extra = [{nmsg, N2}],
  L2 = handle_msg(L, Msg, Extra, []),
  logforward_util:clean_format_cache(),
  State#state{appender = L2, nmsg = N2}.

gc(#state{nmsg = N, garbage_msg = G}) ->
  case N rem G of
    0 -> erlang:garbage_collect(self());
    _ -> pass
  end.

report_msg_n(#state{nmsg = N, sink = Name, throttle = Throttle, report_msg = R}) ->
  case N rem R of
    0 ->
      {message_queue_len, Len} = erlang:process_info(self(), message_queue_len),
      logforward_throttle:report_msg_n(Name, Throttle, Len);
    _ ->
      pass
  end.

handle_msg([], _Msg, _Extra, Acc) -> Acc;
handle_msg([{Name, #appender{mod = Mod, level = LevelLimit, state = State} = Appender} | L], #logforward_msg{level = Level} = Msg, Extra, Acc) ->
  {ok, State2} =
    case ?LEVEL2INT(Level) >= ?LEVEL2INT(LevelLimit) of
      true ->
        Mod:handle_msg(Msg, Extra, State);
      false ->
        {ok, State}
    end,
  Appender2 = Appender#appender{state = State2},
  handle_msg(L, Msg, Extra, [{Name, Appender2} | Acc]).

terminate_appender([], _Reason) -> ok;
terminate_appender([{_, #appender{mod = Mod, state = State}} | L], Reason) ->
  Mod:terminate(Reason, State),
  terminate_appender(L, Reason).





