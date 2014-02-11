%%%-------------------------------------------------------------------
%%% @author qingliang
%%% @copyright (C) 2014, <mingchao>
%%% @doc
%%%
%%% @end
%%% Created : 11. 二月 2014 21:56
%%%-------------------------------------------------------------------
-author("qingliang").

%% 日志记录
%% 程序日志记录宏
-define(DEV(Format, Args),
    mslog_logger_dyn:dev(node(), ?MODULE, ?LINE, Format, Args)).
-define(DEV(D),?DEV(D, [])).

-define(DEBUG(Format, Args),
    mslog_logger_dyn:debug_msg(node(), ?MODULE,?LINE,Format, Args)).
-define(DEBUG(D), ?DEBUG(D, [])).

-define(INFO_MSG(Format, Args),
    mslog_logger_dyn:info_msg(node(), ?MODULE,?LINE,Format, Args)).
-define(INFO_MSG(D), ?INFO_MSG(D, [])).

-define(WARNING_MSG(Format, Args),
    mslog_logger_dyn:warning_msg( node(), ?MODULE,?LINE,Format, Args)).
-define(WARNING_MSG(D), ?WARNING_MSG(D, [])).

-define(ERROR_MSG(Format, Args),
    mslog_logger_dyn:error_msg( node(), ?MODULE,?LINE,Format, Args)).
-define(ERROR_MSG(D), ?ERROR_MSG(D, [])).

-define(CRITICAL_MSG(Format, Args),
    mslog_logger_dyn:critical_msg( node(), ?MODULE,?LINE,Format, Args)).
-define(CRITICAL_MSG(D), ?CRITICAL_MSG(D, [])).
