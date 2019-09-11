-module(logforward_sink).
-author('alking').
-include("logforward.hrl").
-behaviour(gen_server).

%% API
-export([
  start_link/3,
  set_cut_level/2,
  set_appender_level/3,
  msg/2
]).

-export([]).

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
  name,
  options,
  cut_level,
  nmsg,
  appender = [] % {Name,#appender{}}
}).

start_link(Name, Options, Appends) when is_atom(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name, Options, Appends], []).

msg(Name, #logforward_msg{level = Level} = Msg) ->
  CutLevel = logforward_util:get({Name, ?CONFIG_CUT_LEVEL}, ?SINK_CUT_LEVEL_DEFAULT),
  case ?LEVEL2INT(Level) >= ?LEVEL2INT(CutLevel) of
    true ->
      gen_server:cast(Name, {msg, Msg});
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
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name, Options, Appends]) ->
  L = install_appender(Appends, Name, Options, []),
  CutLevel = proplists:get_value(?CONFIG_CUT_LEVEL, Options, ?SINK_CUT_LEVEL_DEFAULT),
  logforward_util:set({Name, ?CONFIG_CUT_LEVEL}, CutLevel),
  {ok, #state{name = Name, options = Options, cut_level = CutLevel, appender = L, nmsg = 0}}.

handle_call({set_cut_level, Level}, _From, #state{name = Name} = State) ->
  logforward_util:set({Name, ?CONFIG_CUT_LEVEL}, Level),
  {reply, ok, State#state{cut_level = Level}};
handle_call({set_appender_level, Appender, Level}, _From, #state{appender = L} = State) ->
  L2 = case lists:keyfind(Appender, 1, L) of
         {_, #appender{} = E} ->
           E2 = E#appender{level = Level},
           lists:keyreplace(Appender, 1, L, {Appender, E2});
         false ->
           L
       end,
  {reply, ok, State#state{appender = L2}};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({msg, Msg}, #state{appender = L, nmsg = N} = State) ->
  N2 = N + 1,
  Extra = [{nmsg, N2}],
  L2 = handle_msg(L, Msg, Extra, []),
  logforward_util:clean_format_cache(),
  {noreply, State#state{appender = L2, nmsg = N2}};

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





