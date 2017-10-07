-module(myapp_web).
-export([start/1]).

-include("settings.hrl").

start(Port) ->
    ?LOG_INFO("~p", ["start myapp_web"]),
    spawn(
        fun() ->
            {ok, Sock} = gen_tcp:listen(Port, [{active, false}]),
            loop(Sock)
        end
    ).

loop(Sock) ->
    ?LOG_INFO("~p", ["get request"]),
    {ok, Conn} = gen_tcp:accept(Sock),
    Handler = spawn(
        fun() ->
            handle(Conn)
        end
    ),
    gen_tcp:controlling_process(Conn, Handler),
    loop(Sock).

handle(Conn) ->
    gen_tcp:send(Conn, response("zhifu")),
    gen_tcp:close(Conn).

response(Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(
        io_lib:fwrite(
            "HTTP/1.0 200 Ok\nConnect-Type: text/html\nContent-length: ~p\n\n~s", [size(B), B]
        )
    ).
