%%%-------------------------------------------------------------------
%%% @author qingliang
%%% @copyright (C) 2014, <mingchao>
%%% @doc 通用宏定义
%%%
%%% @end
%%% Created : 2014.2.11
%%%-------------------------------------------------------------------
-author("qingliang").

-include("mslog_macro.hrl").

%% syntax similar with ?: in c
-ifndef(IF).
-define(IF(C, T, F), (case (C) of true -> (T); false -> (F) end)).
-endif.

-ifndef(ASSERT).
-define(ASSERT(C, T), ?IF(C, ok, erlang:throw(T))).
-endif.

%% 格式转化
-define(B2S(B), (erlang:binary_to_list(B))).
-define(S2B(S), (erlang:list_to_binary(S))).
-define(N2S(N), erlang:integer_to_list(N)).
-define(S2N(S), erlang:list_to_integer(S)).
-define(N2B(N), ?S2B(integer_to_list(N))).
-define(B2N(B), erlang:list_to_integer(?B2S(B))).

%%日志相关
-define(PRINT(Format, Args),
    io:format(Format, Args)).

-define(NONE, none).

%% 定义单元测试相关宏
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% 异常捕获相关
-define(TRY_CATCH(Expression), ?TRY_CATCH(Expression,ErrType,ErrReason)).
-define(TRY_CATCH(Expression,Tip,ErrType,ErrReason),
    try
        Expression
    catch
        _:ErrReason ->
            ?ERROR_MSG("~ts: Reason=~w~n,Stacktrace=~p", [Tip,ErrReason,erlang:get_stacktrace()])
    end).
-define(TRY_CATCH(Expression,ErrType,ErrReason),
    try
        Expression
    catch
        _:ErrReason ->
            ?ERROR_MSG("MapID=~w,Reason=~w~n,Stacktrace=~p", [map_server:get_map_id(),ErrReason,erlang:get_stacktrace()])
    end).
-define(TRY_CATCH(Expression,ErrReason),
    try
        Expression
    catch
        _:ErrReason ->
            ?ERROR_MSG("Reason=~w~n,Stacktrace=~p", [ErrReason,erlang:get_stacktrace()])
    end).

%% gen_server 安全处理
-define(DO_HANDLE_INFO(Info,State),
    try
        do_handle_info(Info)
    catch _:Reason ->
        ?ERROR_MSG("~nInfo:~p~nState:~w~nReason: ~p~nstrace:~p", [Info,State, Reason, erlang:get_stacktrace()])
    end).


-define(DO_HANDLE_CAST(Info,State),
    try
        do_handle_cast(Info)
    catch _:Reason ->
        ?ERROR_MSG("~nInfo:~p~n State:~w~nReason: ~w~nstrace:~p", [Info,State, Reason, erlang:get_stacktrace()])
    end).

-define(DO_HANDLE_CAST_STATE(Info,State),
    try
        do_handle_cast(Info, State)
    catch _:Reason ->
        ?ERROR_MSG("Info:~w~n State:~w~nReason: ~w~nstrace:~p", [Info,State, Reason, erlang:get_stacktrace()]) ,
        State
    end).

-define(DO_HANDLE_CALL(Request,State),
    try
        do_handle_call(Request)
    catch _:Reason ->
        ?ERROR_MSG("~nRequest:~p~nState:~w~nReason: ~p~nstrace:~p", [Request,State, Reason, erlang:get_stacktrace()])
    end).

-define(DO_HANDLE_CALL_STATE(Request,State),
    try
        do_handle_call(Request, State)
    catch _:Reason ->
        ?ERROR_MSG("~nRequest:~p~nState:~w~nReason: ~p~nstrace:~p", [Request,State, Reason, erlang:get_stacktrace()])
    end).

-define(DO_HANDLE_INFO_STATE(Info, State),
    try
        do_handle_info(Info, State)
    catch _:Reason ->
        ?ERROR_MSG("~nInfo:~w~nState:~w~nReason: ~w~nstrace:~p", [Info,State, Reason, erlang:get_stacktrace()]) ,
        State
    end).

-define(DEBUG_HANDLE_INFO(Req,State),
    try
        {Fun,Args} = Req,
        R = erlang:apply(?MODULE,Fun, Args),
        ?ERROR_MSG("apply result=~w",[R])
    catch _:Reason ->
        ?ERROR_MSG("DEBUG_HANDLE_INFO error,Req:~w~n,State:~w~n, Reason: ~w~n, strace:~p", [Req,State, Reason, erlang:get_stacktrace()])
    end,
    {noreply, State}).


