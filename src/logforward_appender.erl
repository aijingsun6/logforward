-module(logforward_appender).
-author('alking').
-include("logforward.hrl").
%% 初始化
-callback init(Sink :: atom(), Name :: atom(), Args :: term()) -> {ok, State :: term()}.
%% 处理一条消息 proplists:: [ {nmsg,N |..]
-callback handle_msg(Msg :: term(), Extra :: proplists:proplist(), State :: term()) -> {ok, State :: term()}.
%% 结束
-callback terminate(Reason :: term(), State :: term()) -> term().
%% 执行GC 释放内存，因为很多是异步操作，所以有额外的PID消耗内存
-callback gc(State :: term()) -> ok.

-optional_callbacks([gc/1]).