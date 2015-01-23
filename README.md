mslog
=====

a simple and easy to use erlang log app

一个简单易用的erlang 日志 app

How to use
------

## add to you project

Add mslog to your rebar.config or just clone it directly

    {deps, [{mslog, ".*", {git, "git://github.com/qingliangcn/mslog.git", master}}]}.
    
or

    git clone git://github.com/qingliangcn/mslog.git


## How to init

add this code to you x_app.erl

    -include("mslog.hrl").

then just call mslog:set/2, mslog:set/3 or mslog:set/4

for example:

log file to /data/logs/test.log

    mslog:set(error, "/data/logs/", test).
    mslog:set("/data/logs/", test). %% default log level : error

log file to /data/logs/test_year_month_day.log

    mslog:set(error, "/data/logs/", test, true).

## log levels

no_log/critical/error/warning/info/debug/dev

## write log

#### Macros:

    ?ERROR_MSG(Format, Args);
    
    ?DEBUG(Format, Args);
    
    ?DEV(Format, Args);
    
    ?WARNING_MSG(Format, Args);
    
    ?CRITICAL_MSG(Format, Args);
    
    ?INFO_MSG(Format, Args);
    
#### Module funcs: (todo)


## notice: 

the mslog app also log the sasl logs.


