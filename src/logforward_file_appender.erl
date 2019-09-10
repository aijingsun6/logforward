-module(logforward_file_appender).
-author('alking').
-include("logforward.hrl").
-behaviour(logforward_appender).

-export([
  init/3,
  handle_msg/2,
  terminate/2
]).

-record(state, {
  sink,
  name,
  level,
  nmsg,
  formatter,
  formatter_config,

  dir,
  file_pattern_conf,
  file_rotate_type,
  file_rotate_size,
  file_max,
  file_nth
}).

init(Sink, Name, Options) ->
  Level = proplists:get_value(?CONFIG_LEVEL, Options, ?APPENDER_LEVEL_DEFAULT),
  Formatter = proplists:get_value(?APPENDER_CONF_FORMATTER, Options, ?APPENDER_FORMATTER_DEFAULT),
  FormatterConf = case lists:keyfind(?APPENDER_FORMAT_PATTERN, 1, Options) of
                    {_, Str} -> Formatter:parse_pattern(Str);
                    false -> ?APPENDER_FORMATTER_CONFIG_DEFAULT
                  end,
  Dir = proplists:get_value(?CONFIG_DIR, Options, ?FILE_APPENDER_DIR_DEFAULT),
  FilePatternConf = case lists:keyfind(?CONFIG_FILE_PATTERN, 1, Options) of
                      {_, FileStr} -> Formatter:parse_pattern(FileStr);
                      false -> ?FILE_APPENDER_PATTERN_CONF(lists:flatten(io_lib:format("~p.~p", [Sink, Name])))
                    end,
  FileRotateType = proplists:get_value(?CONFIG_FILE_ROTATE_TYPE, Options, ?FILE_APPENDER_FILE_ROTATE_TYPE_DEFAULT),
  FileRotateSize = proplists:get_value(?CONFIG_FILE_ROTATE_SIZE, Options, ?FILE_APPENDER_FILE_ROTATE_SIZE_DEFAULT),
  FileMax = proplists:get_value(?CONFIG_FILE_MAX, Options, ?FILE_APPENDER_FILE_MAX_DEFAULT),
  {ok, #state{
    sink = Sink,
    name = Name,
    level = Level,
    nmsg = 0,
    formatter = Formatter, formatter_config = FormatterConf,
    dir = Dir,
    file_pattern_conf = FilePatternConf,
    file_rotate_type = FileRotateType,
    file_rotate_size = FileRotateSize,
    file_max = FileMax
  }}.

handle_msg(_Msg, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.