-module(logforward_appender).
-author('alking').
-include("logforward.hrl").

-callback init(Sink :: atom(), Name :: atom(), Args :: term()) -> {ok, State :: term()}.

-callback handle_msg(Msg :: term(), Extra :: proplists:proplist(), State :: term()) -> {ok, State :: term()}.

-callback terminate(Reason :: term(), State :: term()) -> term().

