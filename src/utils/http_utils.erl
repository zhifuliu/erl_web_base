-module(http_utils).

-include("../settings.hrl").

-export([analysisRequest/1]).

-export([response/1, doRecv/1, doSend/2, errorResponse/2]).

% 用途：解析请求；返回数据
% 解析请求：传入请求数据，解析成一个 tuple，如果有错误，返回 {error, Reasong}, 否则返回 {ok, Method, Request} Request 是一个 tuple
% 只处理 post 和 get 请求，其他的返回错误；数据也只处理 json 数据

% 解析请求行
getRequestLine(RequestLine) ->
    SplitRequestLine = re:split(RequestLine, " "),
    case erlang:length(SplitRequestLine) of
        3 ->
            [Method, Url, Version | _] = SplitRequestLine,
            {string:to_lower(binary_to_list(Method)), binary_to_list(Url), binary_to_list(Version)};
        _ ->
            {error, "请求行解析错误，不是三个数据"}
    end.

% 解析请求头，解析成映射组
getHeaderData(HeaderLines, OldMap) when erlang:length(HeaderLines) > 0 ->
    [CurrentLine | OtherLines] = HeaderLines,
    [Key, Value | _] = re:split(CurrentLine, ": "),
    getHeaderData(OtherLines, OldMap#{string:to_lower(binary_to_list(Key)) => binary_to_list(Value)});
getHeaderData([], Map) ->
    Map.

% 通过请求字符串得到请求参数 map
getParamsMap(Params, OldMap) when erlang:length(Params) > 0 ->
    [CurrentParam | OtherParams] = Params,
    [Key, Value | _] = re:split(CurrentParam, "="),
    getParamsMap(OtherParams, OldMap#{binary_to_list(Key) => binary_to_list(Value)});
getParamsMap([], Map) ->
    Map.

% 解携请求参数，解析成映射组
getRequestParams(Method, RequestUrl, RequestBody) ->
    case Method of
        "get" ->
            [Method1, ParamsUrl | _] = re:split(RequestUrl, "\\?"),
            {ok, binary_to_list(Method1), getParamsMap(re:split(ParamsUrl, "&"), #{})};
        "post" ->
            [ParamsUrl | _] = RequestBody,
            {ok, RequestUrl, getParamsMap(re:split(ParamsUrl, "&"), #{})};
        _ ->
            {error, "当前请求方法不支持"}
    end.

% 将 tcp 获取到的 http 请求解析成一个 erlang 数据结构
analysisRequest(Socket) ->
    case doRecv(Socket) of
        {ok, HttpData} ->
            % 分别获取 http 请求中的请求行、请求头、请求数据
            [Header | RequestBody] = re:split(HttpData, "\r\n\r\n"),
            [RequestLine | HeaderLines] = re:split(Header, "\r\n"),

            % 解析请求行
            case getRequestLine(RequestLine) of
                {Method, RequestUrl, HttpVersion} ->
                    % 解析请求头数据：解析成映射诅
                    HeaderMap = getHeaderData(HeaderLines, #{}),

                    % 获取请求参数，区分 GET 和 POST 请求的参数位置
                    case getRequestParams(Method, RequestUrl, RequestBody) of
                        {ok, RequestUrl1, ParamsMap} ->
                            {ok, Method, #{url => RequestUrl1, httpVersion => HttpVersion, headerParams => HeaderMap, requestParams => ParamsMap}};
                        {error, Reason} ->
                            % 当前 http 请求方法不支持
                            exit(Reason)
                    end;
                {error, Reason} ->
                    % 解析请求行错误
                    {error, Reason}
            end;
        {error, Reason} ->
            % 从 套接字获取请求数据失败
            {error, Reason}
    end.


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
            {ok, Data};
        {error, Reason} ->
            {error, Reason}
    end.

% construct HTML for failure message
errorResponse(LogReq, Reason) ->
    "<html><head><title>Request Failed</title></head><body>\n" ++
    "<h1>Request Failed</h1>\n" ++ "Your request to " ++ LogReq ++
    " failed due to: " ++ Reason ++ "\n</body></html>\n".
