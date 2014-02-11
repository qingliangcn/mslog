-module(mslog_app).

-behaviour(application).

-include("mslog.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, PID} = mslog_sup:start_link(),
    gen_event:add_handler(error_logger, mslog_logger_h, []),
    mslog_loglevel:set(error),
    {ok, PID}.

stop(_State) ->
    ok.
