-module(mslog_app).

-behaviour(application).

-include("mslog.hrl").

%% Application callbacks
-export([start/2, stop/1, set_param/3, set_param/4]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% @doc 设置参数
%% @param ErrorLogLevel 日志级别 0-6
%%     {0, no_log, "No log"}
%%     {1, critical, "Critical"}
%%     {2, error, "Error"}
%%     {3, warning, "Warning"}
%%     {4, info, "Info"}
%%     {5, debug, "Debug"}
%%     {6, dev, "Dev"}
%% @param BaseDir 日志的存储目录
%% @param FileBaseName 基本文件名
set_param(ErrorLogLevel, BaseDir, FileBaseName) ->
    set_param(ErrorLogLevel, BaseDir, FileBaseName, false).

%% @param IsMf 是否按照日期分为多个日志文件
set_param(ErrorLogLevel, BaseDir, FileBaseName, IsMf) ->
    application:ensure_started(sasl),
    application:ensure_started(mslog),
    mslog_loglevel:set(ErrorLogLevel),
    {ok, _} = mslog_logger:start(mslog_sup, [BaseDir, FileBaseName, IsMf]),
    ok.

start(_StartType, _StartArgs) ->
    {ok, PID} = mslog_sup:start_link(),
    gen_event:add_handler(error_logger, mslog_logger_h, []),
    mslog_loglevel:set(error),
    {ok, PID}.

stop(_State) ->
    ok.
