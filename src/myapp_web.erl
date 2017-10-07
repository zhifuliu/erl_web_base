-module(myapp_web).
-export([start/1]).

-include("settings.hrl").

start(Port) ->
    ?LOG_INFO("~p", ["start myapp_web"]),
    % 开启一个新线程来监听指定端口
    spawn(
        fun() ->
            {ok, Sock} = gen_tcp:listen(Port, [{active, false}]),
            % 监听成功后，交由 loop 函数来执行监听
            loop(Sock)
        end
    ).

loop(Sock) ->
    % 监听客户端的请求，每来一个请求，改变本次请求的控制进程（新开的线程），交由 handle 来处理
    ?LOG_INFO("~p", ["get request"]),
    {ok, Conn} = gen_tcp:accept(Sock),
    Handler = spawn(
        fun() ->
            handle(Conn)
        end
    ),
    % 改变套接字控制进程，将本次套接字交由 Handler 处理，Handler 新开了一个线程，所以，可以立马接收下一个请求
    gen_tcp:controlling_process(Conn, Handler),
    loop(Sock).

handle(Conn) ->
    % 每一个请求都会转到本函数来处理
    gen_tcp:send(Conn, response("zhifu")),
    gen_tcp:close(Conn).

response(Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(
        io_lib:fwrite(
            "HTTP/1.0 200 Ok\nConnect-Type: text/html\nContent-length: ~p\n\n~s", [size(B), B]
        )
    ).
