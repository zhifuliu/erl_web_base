-module(http_utils).

-include("settings.hrl").

-export([getRequest/1]).

-export([response/1, doRecv/1, doSend/2, errorResponse/2]).

% 用途：解析请求；返回数据
% 解析请求：传入请求数据，解析成一个 tuple，如果有错误，返回 {error, Reasong}, 否则返回 {ok, Method, Request} Request 是一个 tuple
% 只处理 post 和 get 请求，其他的返回错误；数据也只处理 json 数据

% 解析请求行
getRequestLine(HeaderLine) ->
    ItemList = re:split(HeaderLine, " "),
    [Method | _] = lists:sublist(ItemList, 1, 1),
    [Url | _] = lists:sublist(ItemList, 2, 1),
    [Version | _] = lists:sublist(ItemList, 3, 1),
    {binary_to_list(Method), binary_to_list(Url), binary_to_list(Version)}.

% 解析请求头
getHeaderData(HeaderData) ->
    ok,

getRequest(Socket) ->
    Data = http_utils:doRecv(Socket),
    % 分别获取 http 请求中的请求行、请求头、请求数据
    [Header | BodyData] = re:split(Data, "\r\n\r\n"),
    [RequestLine | HeaderData] = re:split(Header, "\r\n"),
    % ?LOG_INFO("request line. type: ~p ; data: ~p", [tools:getVariableType(RequestLine), RequestLine]),
    % ?LOG_INFO("header. type: ~p ; data: ~p", [tools:getVariableType(HeaderData), HeaderData]),
    % ?LOG_INFO("body. type: ~p ; data: ~p", [tools:getVariableType(BodyData), BodyData]),

    % 解析请求行
    {Method, RequestUrl, HttpVersion} = getRequestLine(RequestLine),

    % 解析请求头数据
    getHeaderData(HeaderData),

    % lists:foreach(
    %     fun(Item) ->
    %         Item1 = binary_to_list(Item),
    %         ?LOG_INFO("type: ~p item: ~p", [tools:getVariableType(Item1), Item1])
    %     end, SplitData
    % ),
    % ?LOG_INFO("split Data:~p~n", [SplitData]),
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
