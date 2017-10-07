-module(myapp_web).
-export([start/1]).

-include("settings.hrl").

start(Port) ->
    ?LOG_INFO("start myapp_web", []),
    % 开启一个新线程来监听指定端口
    case gen_tcp:listen(Port, [binary, {packet, 4}, {reuseaddr, true}, {active, once}]) of
        {ok, Listen} ->
            spawn(fun() -> accept(Listen) end);
            % accept(Listen);
        {error, Reason} ->
            ?LOG_INFO("~p", [Reason]),
            Reason
    end.
    % {ok, Listen} = gen_tcp:listen(Port, [binary, {reuseaddr, true}, {active, true}]),
    % {ok, Socket} = gen_tcp:accept(Listen),
    % loop(Socket).

accept(Listen) ->
    ?LOG_INFO("accept", []),
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            Handler = spawn(fun() -> loop(Socket) end),
            gen_tcp:controlling_process(Socket, Handler);
        {error, Reason} ->
            ?LOG_INFO("~p", [Reason]),
            Reason
    end,
    accept(Listen).

loop(Socket) ->
    receive
        {tcp, Socket, Data} ->
            ?LOG_INFO("received: ~p", [Data]),
            GenResult = gen_server:call(visitors_dict, {add}),
            ?LOG_INFO("GenResult: ~p", [GenResult]),
            gen_tcp:send(Socket, response(integer_to_list(GenResult))),
            loop(Socket);
        {tcp_closed, Socket} ->
            ?LOG_INFO("[~p] tcp_closed", [Socket]),
            gen_tcp:close(Socket);
        {tcp_error, Socket, Reason} ->
            ?LOG_INFO("[~p] tcp_error: ~p", [Socket, Reason]),
            gen_tcp:close(Socket);
        _Other ->
            ?LOG_INFO("other", []),
            gen_tcp:close(Socket)
    after 10000 ->
        ?LOG_INFO("timeout", []),
        gen_tcp:close(Socket)
    end.

response(Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(
        io_lib:fwrite(
            "HTTP/1.0 200 Ok\nConnect-Type: text/html\nContent-length: ~p\n\n~s", [size(B), B]
        )
    ).
