mslog
=====

a simple and easy to use erlang log app

一个简单易用的erlang 日志 app

How to use
------

* Add mslog to your rebar.config or just clone it directly

   将mslog加入到你的rebar.config的deps中,或者直接clone一份到本地


* How to init: call mslog_app:set_params/3 or mslog_app:set_params/4

for example:

log file to /data/logs/test.log

    mslog_app:set_params(error, "/data/logs/", test).

log file to /data/logs/test_year_month_day.log

    mslog_app:set_params(error, "/data/logs/", test, true).

* log levels: no_log/critical/error/warning/info/debug/dev

* write log:

Macros:

    ?ERROR_MSG(Format, Args);
    
    ?DEBUG(Format, Args);
    ?DEV(Format, Args);
    ?WARNING_MSG(Format, Args);
    ?CRITICAL_MSG(Format, Args);
    ?INFO_MSG(Format, Args);


* notice: the mslog app also log the sasl logs.

