% debug < info < warn < error < fatal < none

-define(LOGFORWARD, logforward).

-define(DEFAULT_SINK, logforward_sink).

-define(LOG_LEVEL_DEBUG, debug).
-define(LOG_LEVEL_INFO, info).
-define(LOG_LEVEL_WARN, warn).
-define(LOG_LEVEL_ERROR, error).
-define(LOG_LEVEL_FATAL, fatal).
-define(LOG_LEVEL_NONE, none).


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

-define(LOG_ENABLE(Level, Limit), Level >= Limit).

-define(CONFIG_CUT_LEVEL, cut_level).
-define(CONFIG_LEVEL, level).
-define(CONFIG_LOG_DIR, dir).

-record(logforward_msg, {
  level,
  metadata,
  format,
  args
}).