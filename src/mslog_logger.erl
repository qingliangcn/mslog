%%%-------------------------------------------------------------------
%%% @author QingliangCn <>
%%% @copyright (C) 2010, QingliangCn
%%% @doc 用于写所有Server的运行时日志
%%%
%%% @end
%%% Created : 28 Jun 2010 by QingliangCn <>
%%%-------------------------------------------------------------------
-module(mslog_logger).

-behaviour(gen_server).

-include("mslog.hrl").

%% API
-export([
         start/2,
         start_link/3
        ]).

-export([notify/1, single_notify/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {fd}).

%%--------------------------------------------------------------------
%% 宏定义
%%--------------------------------------------------------------------

%% 堵塞消息的上限值
-define(MAX_PENDING_MSG, 100000).

start(Sup, [BaseDir, FileBaseName, IsMf]) ->
    {ok, _} = supervisor:start_child(Sup, {?MODULE, {?MODULE, start_link, [BaseDir, FileBaseName, IsMf]},
                                                 transient, brutal_kill, worker, [?MODULE]}).


start_link(BaseDir, FileBaseName, IsMf) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [BaseDir, FileBaseName, IsMf], []).

notify(Event) ->
    case erlang:whereis(?MODULE) of
        undefined ->
            %%可能需要打印出来？
            ignore;
        PID ->
            PID ! {event, Event}
    end.

single_notify(Event) ->
    case erlang:whereis(?MODULE) of
        undefined ->
            %%可能需要打印出来？
            ignore;
        PID ->
            PID ! {single_file_event, Event}
    end.


%%%===================================================================
init([BaseDir, FileBaseName, IsMf]) ->
    set_params({BaseDir, FileBaseName, IsMf}),
    File = make_log_file(BaseDir,FileBaseName, IsMf),
    trucate_file_at_next_day(),
    {ok, #state{fd=File}}.


%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
handle_info({event, Event}, State) ->
    {_, Len} = erlang:process_info(erlang:self(), message_queue_len),
    case Len > ?MAX_PENDING_MSG of
        true ->
            do_notify(Len),
            do_flush_message(Len);
        false ->
            write_event(State#state.fd, {erlang:localtime(), Event})
    end,
    {noreply, State};
handle_info({single_file_event, Event}, State) ->    
    ?TRY_CATCH(do_single_file_event(Event)),    
    {noreply, State};
handle_info(truncate_file, State) ->
    {BaseDir, FileBaseName, IsMf} = get_params(),
    File = make_log_file(BaseDir, FileBaseName, IsMf),
    ?TRY_CATCH(trucate_file_at_next_day()),
    {noreply, State#state{fd=File}};
handle_info(Info, State) ->
    ?ERROR_MSG("~ts:~w", ["未知的消息", Info]),
    {noreply, State}.


%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

do_single_file_event(Event) ->
    {BaseDir, _FileBaseName, _IsMf} = get_params(),
    {FileName, {{{Y, M, D}, {H, I, S}}, Node, PID, Module, Line, Format, Args}} = Event,
    Content = io_lib:format("===DBG *** ~w--~.2.0w-~.2.0p ~.2.0p:~.2.0p:~.2.0p ===( ~s:~p:~s: ~.5p) ~s~n", [Y, M, D, H, I, S, Node, PID, Module, Line, io_lib:format(Format, Args)]),    
    file:write_file(lists:concat([BaseDir, "/", FileName, ".log"]), Content,  [append, delayed_write]),
    ok.

do_flush_message(0) ->
    ok;
do_flush_message(N) ->
    receive 
        _R ->
            ok
    after 0 ->
            ok
    end,
    do_flush_message(N-1).

do_notify(Len) ->
    ?ERROR_MSG("mservice log queue too large :~p~n", [Len]).

do_write(Fd, Time, Type, Format, Args) ->
    {{Y,Mo,D},{H,Mi,S}} = Time,
    Time2 = io_lib:format("==== ~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w ===",
		  [Y, Mo, D, H, Mi, S]),
    file:write_file(Fd, Type, [append, delayed_write]),
    file:write_file(Fd, Time2, [append, delayed_write]),
    try
        M = io_lib:format(Format, Args),
        file:write_file(Fd, M, [append, delayed_write]),
        ok
    catch E:Error ->
        io:format("log error ~p ~p ~p ~p", [E, Error, Format, Args])
    end.

% Copied from erlang_logger_file_h.erl
write_event(Fd, {Time, {error, _GL, {_Pid, Format, Args}}}) ->
    [L] = io_lib:format("~s", ["ERROR: "]),
    do_write(Fd, Time, L, Format, Args);

write_event(Fd, {Time, {emulator, _GL, Chars}}) ->
    T = write_time(Time),
    case catch io_lib:format(Chars, []) of
	S when is_list(S) ->
	    file:write_file(Fd, io_lib:format(T ++ S, []), [append, delayed_write]);
	_ ->
	    file:write_file(Fd, io_lib:format(T ++ "ERROR: ~p ~n", [Chars]), [append, delayed_write])
    end;


write_event(Fd, {Time, {info, _GL, {Pid, Info, _}}}) ->
    T = write_time(Time),
    file:write_file(Fd, io_lib:format(T ++ add_node("~p~n",Pid), [Info]), [append, delayed_write]);


write_event(Fd, {Time, {error_report, _GL, {Pid, std_error, Rep}}}) ->
    T = write_time(Time),
    S = format_report(Rep),
    file:write_file(Fd, io_lib:format(T ++ S ++ add_node("", Pid), []), [append, delayed_write]);


write_event(Fd, {Time, {info_report, _GL, {Pid, std_info, Rep}}}) ->
    T = write_time(Time, "INFO REPORT"),
    S = format_report(Rep),
    file:write_file(Fd, io_lib:format(T ++ S ++ add_node("", Pid), []), [append, delayed_write]);


write_event(Fd, {Time, {info_msg, _GL, {_Pid, Format, Args}}}) ->
    [L] = io_lib:format("~s", ["INFO REPORT"]),
    do_write(Fd, Time, L, Format, Args);

write_event(Fd, {Time, {info_report, _GL, {_PID, progress, Detail}}}) ->
    [L] = io_lib:format("~ts", ["OTP progress"]),
    do_write(Fd, Time, L, "~n~p~n~n", [Detail]);


write_event(_, Info) ->
    io:format("~ts: ~p", ["Unknow msg", Info]),
    ok.

format_report(Rep) when is_list(Rep) ->
    case string_p(Rep) of
	true ->
	    io_lib:format("~s~n",[Rep]);
	_ ->
	    format_rep(Rep)
    end;
format_report(Rep) ->
    io_lib:format("~p~n",[Rep]).

format_rep([{Tag,Data}|Rep]) ->
    io_lib:format("    ~p: ~p~n",[Tag,Data]) ++ format_rep(Rep);


format_rep([Other|Rep]) ->
    io_lib:format("    ~p~n",[Other]) ++ format_rep(Rep);


format_rep(_) ->
    [].

add_node(X, Pid) when node(Pid) /= node() ->
    lists:concat([X,"** at node ",node(Pid)," **~n"]);
add_node(X, _) ->
    X.

string_p([]) ->
    false;
string_p(Term) ->
    string_p1(Term).

string_p1([H|T]) when is_integer(H), H >= $\s, H < 255 ->
    string_p1(T);
string_p1([$\n|T]) -> string_p1(T);
string_p1([$\r|T]) -> string_p1(T);
string_p1([$\t|T]) -> string_p1(T);
string_p1([$\v|T]) -> string_p1(T);
string_p1([$\b|T]) -> string_p1(T);
string_p1([$\f|T]) -> string_p1(T);
string_p1([$\e|T]) -> string_p1(T);
string_p1([H|T]) when is_list(H) ->
    case string_p1(H) of
	true -> string_p1(T);
	_    -> false
    end;
string_p1([]) -> true;
string_p1(_) ->  false.

write_time(Time) -> write_time(Time, "ERROR REPORT").

write_time({{Y,Mo,D},{H,Mi,S}}, Type) ->
    io_lib:format("~n=~s==== ~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w ===",
		  [Type, Y, Mo, D, H, Mi, S]).


%% @doc 生成日志文件名
make_log_file(BaseDir, FileBaseName, IsMf) ->
    ok = filelib:ensure_dir(BaseDir),
    case IsMf of
        true ->
            {Year, Month, Day} = erlang:date(),
            io_lib:format("~s/~s_~p_~p_~p.log", [BaseDir, FileBaseName, Year, Month, Day]);
        false ->
            io_lib:format("~s/~s.log", [BaseDir, FileBaseName])
    end.

%% @doc 通知服务器在下一个整点刷新日志文件
trucate_file_at_next_day() ->
    {{_, _, _Day}, {H, I, S}} = erlang:localtime(),
    Time = (23-H) * 3600 + ((59 - I) * 60 + (59 - S) + 2) * 1000,
    erlang:send_after(Time, self(), truncate_file).


%%
set_params({BaseDir, FileBaseName, IsMf}) ->
    erlang:put(params, {BaseDir, FileBaseName, IsMf}).
get_params() ->
    erlang:get(params).
