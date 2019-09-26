-define(DEFAULT_SINK, sink).

-define(LOG_LEVEL_DEBUG, debug).
-define(LOG_LEVEL_INFO, info).
-define(LOG_LEVEL_WARN, warn).
-define(LOG_LEVEL_ERROR, error).
-define(LOG_LEVEL_FATAL, fatal).
-define(LOG_LEVEL_NONE, none).
-define(LOG_LEVEL_ALL, [debug, info, warn, error, fatal, none]).

-define(LOG_LEVEL_DEBUG_INT, 1).
-define(LOG_LEVEL_INFO_INT, 2).
-define(LOG_LEVEL_WARN_INT, 3).
-define(LOG_LEVEL_ERROR_INT, 4).
-define(LOG_LEVEL_FATAL_INT, 5).
-define(LOG_LEVEL_NONE_INT, 9).

-define(LEVEL2INT(X), case X of
                        ?LOG_LEVEL_DEBUG -> ?LOG_LEVEL_DEBUG_INT;
                        ?LOG_LEVEL_INFO -> ?LOG_LEVEL_INFO_INT;
                        ?LOG_LEVEL_WARN -> ?LOG_LEVEL_WARN_INT;
                        ?LOG_LEVEL_ERROR -> ?LOG_LEVEL_ERROR_INT;
                        ?LOG_LEVEL_FATAL -> ?LOG_LEVEL_FATAL_INT;
                        ?LOG_LEVEL_NONE -> ?LOG_LEVEL_NONE_INT
                      end).

-define(CONFIG_CUT_LEVEL, cut_level).
-define(SINK_CUT_LEVEL_DEFAULT, info).

-define(CONFIG_THROTTLE, throttle).
%% 当sink的消息队列超过一定数量后，block住
-define(SINK_THROTTLE_DEFAULT, 1024).

%% 每N条消息，进行一次垃圾回收
-define(CONFIG_MSG_PER_GC, msg_per_gc).
-define(SINK_GC_DEFAULT, 1024).

%% 每N条消息，上报消息队列长度
-define(CONFIG_MSG_PER_REPORT, msg_per_report).
-define(SINK_REPORT_DEFAULT, 256).

-define(CONFIG_LEVEL, level).
-define(APPENDER_LEVEL_DEFAULT, info).

-define(APPENDER_CONF_FORMATTER, formatter).
-define(APPENDER_FORMATTER_DEFAULT, logforward_default_formatter).

-define(APPENDER_FORMAT_PATTERN, pattern).
-define(APPENDER_FORMATTER_CONFIG_DEFAULT, [datetime, " [", level, "] - ", msg, eol]).
%%-define(APPENDER_FORMATTER_CONFIG_DEFAULT, [datetime, " [", level, "] - (", module, ",", line, ",", function, ",", function_arity, ") ", msg, eol]).

-record(logforward_msg, {
  datetime,
  timestamp_ms,% in ms
  module,
  line,
  function,
  function_arity,
  level,
  node,
  pid,
  metadata,
  format,
  args
}).