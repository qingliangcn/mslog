%%%-------------------------------------------------------------------
%%% @author qingliang
%%% @copyright (C) 2014, <mingchao>
%%% @doc
%%%
%%% @end
%%% Created : 11. 二月 2014 22:02
%%%-------------------------------------------------------------------
-module(mslog).
-author("qingliang").

%% API
-export([
    set/2,
    set/3,
    set/4
]).

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
%% @param IsMf 是否按照日期分为多个日志文件
set(ErrorLogLevel, BaseDir, FileBaseName, IsMf) ->
    application:start(sasl),
    application:start(mslog),
    mslog_loglevel:set(ErrorLogLevel),
    {ok, _} = mslog_logger:start(mslog_sup, [BaseDir, FileBaseName, IsMf]),
    ok.

set(ErrorLogLevel, BaseDir, FileBaseName) ->
    set(ErrorLogLevel, BaseDir, FileBaseName, false).

set(BaseDir, FileBaseName) ->
    set(error, BaseDir, FileBaseName, false).




