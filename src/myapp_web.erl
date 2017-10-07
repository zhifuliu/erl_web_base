-module(myapp_web).
-export([start/1]).

-include("settings.hrl").

start(Port) ->
    ?LOG_INFO("~p", ["start myapp_web"]),
    % 开启一个新线程来监听指定端口
    spawn(
        fun() ->
            % case gen_tcp:listen(Port, [{active, false}, {reuseaddr, true}]) of
            case gen_tcp:listen(Port, [binary, {packet, 4}, {reuseaddr, true}, {active, true}]) of
                {ok, ListenSocket} ->
                    ?LOG_INFO("start myapp_web success", []),
                    spawn(
                        fun() ->
                            accept(ListenSocket)
                        end
                    );
                {error, Reason} ->
                    ?LOG_INFO("start myapp_web fail, reason:~p", [Reason])
            end
        end
    ).

accept(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            Handler = spawn(
                fun() ->
                    loop(Socket)
                end
            ),
            gen_tcp:controlling_process(Socket, Handler);
        {error, Reason} ->
            ?LOG_INFO("gen_tcp accept error, reason:~p", [Reason])
    end,
    accept(ListenSocket).

loop(Socket) ->
    ?LOG_INFO("loop", []),

    receive
        {tcp, Socket, Bin} ->
            ?LOG_INFO("received: ~p", [Bin]),
            GenResult = gen_server:call(visitors_dict, {add}),
            gen_tcp:send(Socket, response(integer_to_list(GenResult))),
            loop(Socket);
        {tcp_closed, Socket} ->
            ?LOG_INFO("[~p] tcp_closed", [Socket]);
        {tcp_error, Socket, Reason} ->
            ?LOG_INFO("[~p] tcp_error: ~p", [Socket, Reason])
    end.

response(Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(
        io_lib:fwrite(
            "HTTP/1.0 200 Ok\nConnect-Type: text/html\nContent-length: ~p\n\n~s", [size(B), B]
        )
    ).
