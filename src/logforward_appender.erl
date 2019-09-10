-module(logforward_appender).
-author('alking').

-callback init(Args :: term()) -> {ok, State :: term()}.

-callback handle_msg(Msg :: term(), State :: term()) -> {ok, State :: term()}.

-callback terminate(Reason :: term(), State :: term()) -> term().

