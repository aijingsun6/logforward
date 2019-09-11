-module(logforward_file_appender).
-author('alking').
-include("logforward.hrl").
-behaviour(logforward_appender).

-export([
  init/3,
  handle_msg/3,
  terminate/2
]).

-export([
  start/2,
  loop/3
]).

-define(CONFIG_DIR, dir).
-define(DIR_DEFAULT, "logs").

-define(CONFIG_FILE_PATTERN, file_pattern).
-define(CONFIG_ROTATE_TYPE, rotate_type).
-define(CONFIG_ROTATE_SIZE, rotate_size).

-define(ROTATE_TYPE_DATA_SIZE, data_size).
-define(ROTATE_TYPE_MSG_SIZE, msg_size).
-define(ROTATE_TYPE_DEFAULT, ?ROTATE_TYPE_DATA_SIZE).

%% 10M
-define(ROTATE_DATA_SIZE_DEFAULT, 1024 * 1024 * 10).
%% 10w
-define(ROTATE_MSG_SIZE_DEFAULT, 10000 * 10).

%% 最多保留多少文件，默认10个 xxx.0.log .... xxx.l0.log
-define(CONFIG_FILE_MAX, max).
-define(FILE_MAX_DEFAULT, 10).

-record(state, {
  sink,
  name,
  level,
  formatter,
  formatter_config,

  dir,
  file_pattern_conf,
  file_rotate_type,
  file_rotate_size,
  file_max,
  file_acc = 0,
  file_pid
}).

init(Sink, Name, Options) ->
  Level = proplists:get_value(?CONFIG_LEVEL, Options, ?APPENDER_LEVEL_DEFAULT),
  Formatter = proplists:get_value(?APPENDER_CONF_FORMATTER, Options, ?APPENDER_FORMATTER_DEFAULT),
  FormatterConf = case lists:keyfind(?APPENDER_FORMAT_PATTERN, 1, Options) of
                    {_, Str} -> Formatter:parse_pattern(Str);
                    false -> ?APPENDER_FORMATTER_CONFIG_DEFAULT
                  end,
  Dir = proplists:get_value(?CONFIG_DIR, Options, ?DIR_DEFAULT),
  FilePatternConf = case lists:keyfind(?CONFIG_FILE_PATTERN, 1, Options) of
                      {_, FileStr} -> Formatter:parse_pattern(FileStr);
                      false -> file_pattern_conf(Sink, Name)
                    end,
  RotateType = proplists:get_value(?CONFIG_ROTATE_TYPE, Options, ?ROTATE_TYPE_DEFAULT),
  RotateSize = case RotateType of
                 ?ROTATE_TYPE_DATA_SIZE -> proplists:get_value(?CONFIG_ROTATE_SIZE, Options, ?ROTATE_DATA_SIZE_DEFAULT);
                 ?ROTATE_TYPE_MSG_SIZE -> proplists:get_value(?CONFIG_ROTATE_SIZE, Options, ?ROTATE_MSG_SIZE_DEFAULT)
               end,
  FileMax = proplists:get_value(?CONFIG_FILE_MAX, Options, ?FILE_MAX_DEFAULT),
  file:make_dir(Dir),
  State = #state{
    sink = Sink,
    name = Name,
    level = Level,
    formatter = Formatter, formatter_config = FormatterConf,
    dir = Dir,
    file_pattern_conf = FilePatternConf,
    file_rotate_type = RotateType,
    file_rotate_size = RotateSize,
    file_max = FileMax
  },
  State2 = open_file(State),
  {ok, State2}.

handle_msg(Msg, Extra, #state{formatter = Formatter, formatter_config = FormatterConf} = State) ->
  case logforward_util:format_msg_with_cache(Msg, Formatter, FormatterConf, Extra) of
    Str when is_list(Str) ->
      State3 = do_log(Str, State),
      {ok, State3};
    _ ->
      {ok, State}
  end.

terminate(_Reason, #state{file_pid = FP}) when is_pid(FP) ->
  FP ! close,
  ok;
terminate(_Reason, _State) ->
  ok.

file_pattern_conf(Sink, Name) ->
  H = lists:flatten(io_lib:format("~p.~p.", [Sink, Name])),
  [H, date, ".", nth, ".log"].

open_file(#state{dir = Dir, file_pattern_conf = FPC, formatter = Formatter, file_rotate_type = FRT} = State) ->
  FileName = file_name(Dir, Formatter, FPC, [{nth, 0}]),
  FileAcc =
    case FRT of
      ?ROTATE_TYPE_DATA_SIZE -> filelib:file_size(FileName);
      ?ROTATE_TYPE_MSG_SIZE -> file_lines(FileName)
    end,
  Pid = start(FileName, erlang:self()),
  State#state{file_acc = FileAcc, file_pid = Pid}.

file_name(Dir, Formatter, FPC, Opt) ->
  filename:join([Dir, Formatter:format_file(FPC, Opt)]).

file_lines(FileName) ->
  case file:open(FileName, [read, {encoding, utf8}]) of
    {ok, IoDevice} -> file_lines(IoDevice, 0);
    {error, _} -> 0
  end.

file_lines(IoDevice, Acc) ->
  case file:read_line(IoDevice) of
    {ok, _Data} -> file_lines(IoDevice, Acc + 1);
    eof -> file:close(IoDevice), Acc;
    {error, _} -> file:close(IoDevice), Acc
  end.


start(FileName, Parent) ->
  {ok, IoDevice} = file:open(FileName, [append, {encoding, utf8}]),
  erlang:spawn(fun() -> start_i(FileName, IoDevice, Parent) end).

start_i(FileName, IoDevice, Parent) ->
  Ref = erlang:monitor(process, Parent),
  loop(FileName, IoDevice, Ref).

loop(FileName, IoDevice, MonitorRef) ->
  receive
    {append, Str} ->
      catch io:put_chars(IoDevice, Str),
      ?MODULE:loop(FileName, IoDevice, MonitorRef);
    {'DOWN', MonitorRef, _, _, _} ->
      file:close(IoDevice), erlang:demonitor(MonitorRef), ok;
    close ->
      io:format("file ~s close ~n", [FileName]),
      file:close(IoDevice), erlang:demonitor(MonitorRef), ok;
    _ ->
      ?MODULE:loop(FileName, IoDevice, MonitorRef)
  end.

do_log(Str, #state{dir = Dir,
  formatter = Formatter,
  file_pattern_conf = FilePatternConf,
  file_rotate_type = RotateType,
  file_rotate_size = RotateSize,
  file_max = FileMax,
  file_acc = FileAcc,
  file_pid = Pid} = State) ->
  Add = case RotateType of
          ?ROTATE_TYPE_DATA_SIZE ->
            Bin = unicode:characters_to_binary(Str),
            erlang:byte_size(Bin);
          ?ROTATE_TYPE_MSG_SIZE ->
            1
        end,
  case FileAcc < RotateSize of
    false ->
      % rotate file
      Pid ! close,
      rotate(FileMax, FileMax, Dir, FilePatternConf, Formatter),
      FileName = file_name(Dir, Formatter, FilePatternConf, [{nth, 0}]),
      NewPid = start(FileName, erlang:self()),
      NewPid ! {append, Str},
      State#state{file_acc = Add, file_pid = NewPid};
    true ->
      Pid ! {append, Str},
      State#state{file_acc = FileAcc + Add}
  end.


rotate(IDX, MAX, Dir, FilePatternConf, Formatter) when IDX == MAX ->
  % delete file
  FileName = file_name(Dir, Formatter, FilePatternConf, [{nth, IDX}]),
  case filelib:is_file(FileName) of
    true -> file:delete(FileName);
    false -> pass
  end,
  rotate(IDX - 1, MAX, Dir, FilePatternConf, Formatter);
rotate(IDX, MAX, Dir, FilePatternConf, Formatter) ->
  Ori = file_name(Dir, Formatter, FilePatternConf, [{nth, IDX}]),
  Target = file_name(Dir, Formatter, FilePatternConf, [{nth, IDX + 1}]),
  case filelib:is_file(Ori) of
    true ->
      case Ori == Target of
        true -> file:delete(Ori);
        false -> file:rename(Ori, Target)
      end;
    false ->
      pass
  end,
  case IDX > 0 of
    true ->
      rotate(IDX - 1, MAX, Dir, FilePatternConf, Formatter);
    false ->
      % end point
      ok
  end.

