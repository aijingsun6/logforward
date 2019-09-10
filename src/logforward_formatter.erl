-module(logforward_formatter).
-author('alking').
-include("logforward.hrl").

%% 解析字符串成配置
-callback parse_pattern(Pattern :: string()) -> Config :: list().

%% 将日志格式化成 binary
-callback format(Msg :: #logforward_msg{}, Config :: term(), Extra :: proplists:proplist()) -> Str :: string().
