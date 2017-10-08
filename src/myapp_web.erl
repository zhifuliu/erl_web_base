-module(myapp_web).
-export([start/1]).

-include("settings.hrl").

start(Port) ->
    ?LOG_INFO("~p", ["start myapp_web"]),
    % 开启一个新线程来监听指定端口
    spawn(
        fun() ->
            case gen_tcp:listen(Port, [{active, false}, {reuseaddr, true}]) of
                {ok, ListenSocket} ->
                    % 监听成功后，交由 loop 函数来执行监听
                    ?LOG_INFO("start myapp_web success", []),
                    loop(ListenSocket);
                {error, Reason} ->
                    ?LOG_INFO("start myapp_web fail, reason:~p", [Reason])
            end
        end
    ).

loop(ListenSocket) ->
    % 监听客户端的请求，每来一个请求，改变本次请求的控制进程（新开的线程），交由 handle 来处理
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            Handler = spawn(
                fun() ->
                    handle(Socket)
                end
            ),
            % 改变套接字控制进程，将本次套接字交由 Handler 处理，Handler 新开了一个线程，所以，可以立马接收下一个请求
            gen_tcp:controlling_process(Socket, Handler);
        {error, Reason} ->
            ?LOG_INFO("gen_tcp accept error, reason:~p", [Reason])
    end,
    loop(ListenSocket).

handle(Socket) ->
    % 每一个请求都会转到本函数来处理
    Request = do_recv(Socket),
    ?LOG_INFO("Request Data:~p", [Request]),

    % gen_server 实验，这个地方调用 visitors_dict gen_server 中的添加一次访问
    GenResult = gen_server:call(visitors_dict, {add}),
    % ?LOG_INFO("~p", [GenResult]),

    case gen_tcp:send(Socket, response(integer_to_list(GenResult))) of
        ok ->
            ?LOG_INFO("reponse success", []);
        {error, Reason} ->
            ?LOG_INFO("reponse fail, reason:~p", [Reason])
    end,
    gen_tcp:close(Socket).

response(Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(
        io_lib:fwrite(
            "HTTP/1.0 200 Ok\nConnect-Type: text/html\nContent-length: ~p\n\n~s", [size(B), B]
        )
    ).

do_send(Socket, Msg) ->
    case gen_tcp:send(Socket, Msg) of
        ok ->
            ok;
        {error, Reason} ->
            exit(Reason)
    end.

do_recv(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            % binary_to_list(Data
            Data;
        {error, closed} ->
            exit(closed);
        {error, Reason} ->
            exit(Reason)
    end.
