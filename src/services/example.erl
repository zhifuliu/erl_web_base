%%% @author zhifu <491836641@qq.com>
%%% tcp & rpc demo

-module(example).
-behaviour(gen_server).

% self apis
-export([start_link/1, start_link/0, get_count/0, stop/0, start_test/0]).

% gen_server apis : gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).       % 将 SERVER 宏设置为模块名
-define(DEFAULT_PORT, 1055).    % 默认提供 tcp 服务端口

-record(state, {port, lsock, request_count = 0}).   % 用于保存进程状态

% 模块的所有功能都是通过应用编程接口（api）提供给用户（用户才不关心实现细节）。对于通用服务器而言，用户主要完成以下两件事：
% 1 启动服务器进程；
% 2 向进程发消息（并获取应答）

% gen_server 提供了三个主要的库函数来实现这些基本功能：
% 1 gen_server:start_link/4     对应的回调函数     Module:init/1
% 2 gen_server:call/2           对应的回调函数     Module:handle_call/3
% 3 gen_server:cast/2           对应的回调函数     Module:handle_cast2

%% @spec start_link(Port :: integer()) -> {ok, Pid}
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

start_link() ->
    start_link(?DEFAULT_PORT).

get_count() ->
    gen_server:call(?SERVER, gen_count).    % 调用方等待应答

stop() ->
    gen_server:cast(?SERVER, stop).         % 无须等待应答



% gen_server callbacks
init(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    {ok, #state{port = Port, lsock = LSock}, 0}.

handle_call(get_count, _from, State) -> {reply, {ok, State#state.request_count}, State}.

handle_cast(stop, State) -> {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
    do_rpc(Socket, RawData),
    RequestCount = State#state.request_count,
    {normal, State#state{request_count = RequestCount + 1}};
handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {normal, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

% 内部函数
do_rpc(Socket, RawData) ->
    try
        {M, F, A} = split_out_mfa(RawData),
        Result = apply(M, F, A),
        gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result]))
    catch
        _Class:Err ->
            gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Err]))
    end.

split_out_mfa(RawData) ->
    MFA = re:replace(RawData, "\r\n$", "", [{return, list}]),
    {match, [M, F, A]} = re:run(MFA, "(.*):(.*)\s\\((.*)\s*\\)\s*.\s*$", [{capture, [1,2,3], list}, ungreedy]),
    {list_to_atom(M), list_to_atom(F), args_to_terms(A)}.

args_to_terms(RawArgs) ->
    {ok, Toks, _Line} = erl_scan:string("["++ RawArgs ++"]. ", 1),
    {ok, Args} = erl_parse:parse_term(Toks),
    Args.

% test

start_test() -> {ok, _} = tr_server:start_link(1055).
