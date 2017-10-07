-module(myapp_web).
-export([start/2]).

-include("settings.hrl").

start(Port, Doc) ->
    ?LOG_INFO("~p", ["start myapp_web"]),
    spawn(
        fun() ->
            {ok, Sock} = gen_tcp:listen(Port, [{active, false}]),
            loop(Sock, Doc)
        end
    ).

loop(Sock, Doc) ->
    ?LOG_INFO("~p", ["get request"]),
    {ok, Conn} = gen_tcp:accept(Sock),
    Handler = spawn(
        fun() ->
            handle(Conn, Doc)
        end
    ),
    gen_tcp:controlling_process(Conn, Handler),
    loop(Sock, Doc).

handle(Conn, Doc) ->
    ?LOG_INFO("~p", [Doc]),
    % {ok, Data} = file:read_file(Doc),
    % Data = zhifu,
    % gen_tcp:send(Conn, response(binary_to_list(Data))),
    gen_tcp:send(Conn, response("zhifu")),
    gen_tcp:close(Conn).

response(Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(
        io_lib:fwrite(
            "HTTP/1.0 200 Ok\nConnect-Type: text/html\nContent-length: ~p\n\n~s", [size(B), B]
        )
    ).
