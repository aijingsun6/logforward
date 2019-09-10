-module(logforward_appender).
-author('alking').

-callback init(Args :: term()) -> {ok, State :: term()}.

-callback msg(Msg :: term(), State :: term()) -> {ok, State :: term()}.

