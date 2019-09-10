-module(logforward_sink).
-author('alking').
-include("logforward.hrl").
-behaviour(gen_server).

%% API
-export([
  start_link/3,
  msg/2
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
  name,
  options,
  cut_level,
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

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name, Options, Appends]) ->
  L = install_appender(Appends, Options, []),
  CutLevel = proplists:get_value(?CONFIG_CUT_LEVEL, Options, ?SINK_CUT_LEVEL_DEFAULT),
  logforward_util:set({Name, ?CONFIG_CUT_LEVEL}, CutLevel),
  {ok, #state{name = Name, options = Options, cut_level = CutLevel, appender = L}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({msg, Msg}, #state{appender = L} = State) ->
  L2 = handle_msg(L, Msg, []),
  {noreply, State#state{appender = L2}};

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

install_appender([], _SinkOpt, Acc) -> Acc;
install_appender([{Name, Mod, Opt} | L], SinkOpt, Acc) ->
  Opt2 = merge_appender_option(SinkOpt, Opt),
  Level = proplists:get_value(?CONFIG_LEVEL, Opt2, ?APPENDER_LEVEL_DEFAULT),
  {ok, State} = Mod:init(Opt2),
  Appender = #appender{name = Name, mod = Mod, options = Opt2, level = Level, state = State},
  install_appender(L, SinkOpt, [{Name, Appender} | Acc]).

handle_msg([], _Msg, Acc) -> Acc;
handle_msg([{Name, #appender{mod = Mod, level = LevelLimit, state = State} = Appender} | L], #logforward_msg{level = Level} = Msg, Acc) ->
  {ok, State2} =
    case ?LEVEL2INT(Level) >= ?LEVEL2INT(LevelLimit) of
      true ->
        Mod:handle_msg(Msg, State);
      false ->
        {ok, State}
    end,
  Appender2 = Appender#appender{state = State2},
  handle_msg(L, Msg, [{Name, Appender2} | Acc]).

terminate_appender([], _Reason) -> ok;
terminate_appender([{_, #appender{mod = Mod, state = State}} | L], Reason) ->
  Mod:terminate(Reason, State),
  terminate_appender(L, Reason).





