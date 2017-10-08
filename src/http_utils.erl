-module(http_utils).

-include("settings.hrl").

-export([getRequest/1]).

-export([response/1, doRecv/1, doSend/2, errorResponse/2]).

% 用途：解析请求；返回数据
% 解析请求：传入请求数据，解析成一个 tuple，如果有错误，返回 {error, Reasong}, 否则返回 {ok, Method, Request} Request 是一个 tuple

% handleRecv(Header, Other, ResultTuple) ->
%     ?LOG_INFO("Header:~p~n", [Header]),
%     handleRecv()

getRequest(Socket) ->
    Data = http_utils:doRecv(Socket),
    SplitData = re:split(Data, "\r\n"),
    % lists:foreach(
    %     fun(Item) ->
    %         Item1 = binary_to_list(Item),
    %         ?LOG_INFO("type: ~p item: ~p", [tools:getVariableType(Item1), Item1])
    %     end, SplitData
    % ),
    ?LOG_INFO("split Data:~p~n", [SplitData]),
    Data,
    {error, "analysis Request"}.

response(Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(
        io_lib:fwrite(
            "HTTP/1.0 200 Ok\nConnect-Type: text/html\nContent-length: ~p\n\n~s", [size(B), B]
        )
    ).

doSend(Socket, Msg) ->
    case gen_tcp:send(Socket, Msg) of
        ok ->
            ?LOG_INFO("reponse success", []),
            ok;
        {error, Reason} ->
            ?LOG_INFO("reponse fail, reason:~p", [Reason]),
            exit(Reason)
    end.

doRecv(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            % ?LOG_INFO("Request Data type:~p~n", [tools:getVariableType(Data)]),
            % ?LOG_INFO("Request Data:~p~n", [Data]),

            % SplitData = re:split(Data, "\r\n"),
            % ?LOG_INFO("split Data type:~p~n", [tools:getVariableType(SplitData)]),
            % ?LOG_INFO("split Data:~p~n", [SplitData]),
            % {ok,[Cmd|[Name|[Vers|_]]]} = split(Req,"[ \r\n]"),
            % SplitData;
            Data;
        {error, closed} ->
            exit(closed);
        {error, Reason} ->
            exit(Reason)
    end.

% construct HTML for failure message
errorResponse(LogReq, Reason) ->
    "<html><head><title>Request Failed</title></head><body>\n" ++
    "<h1>Request Failed</h1>\n" ++ "Your request to " ++ LogReq ++
    " failed due to: " ++ Reason ++ "\n</body></html>\n".
