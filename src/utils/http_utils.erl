-module(http_utils).

-include("../settings.hrl").

-export([analysisRequest/1]).

-export([response/1, doRecv/1, doSend/2]).

% 用途：解析请求；返回数据
% 解析请求：传入请求数据，解析成一个 tuple，如果有错误，返回 {error, Reasong}, 否则返回 {ok, Method, Request} Request 是一个 tuple
% 只处理 post 和 get 请求，其他的返回错误；数据也只处理 json 数据

% 解析请求行
getRequestLine(RequestLine) ->
    SplitRequestLine = re:split(RequestLine, " "),
    case erlang:length(SplitRequestLine) of
        3 ->
            [Method, Url, Version | _] = SplitRequestLine,
            [_, Url1 | _] = re:split(Url, "/"),
            {ok, string:to_lower(binary_to_list(Method)), binary_to_list(Url1), binary_to_list(Version)};
        _ ->
            {error, ?HTTP_ERROR, "请求行解析错误，不是三个数据"}
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
            case re:split(RequestUrl, "\\?") of
                [RequestUrl1, ParamsUrl | _] ->
                    {ok, binary_to_list(RequestUrl1), getParamsMap(re:split(ParamsUrl, "&"), #{})};
                _ ->
                    {ok, RequestUrl, #{}}
            end;
        "post" ->
            [ParamsUrl | _] = RequestBody,
            % ?LOG_INFO("~p", [erlang:length(binary_to_list(ParamsUrl))]),
            ParamsUrlLength = binary_to_list(ParamsUrl),
            if
                erlang:length(ParamsUrlLength) > 0 ->
                    {ok, RequestUrl, getParamsMap(re:split(ParamsUrl, "&"), #{})};
                true ->
                    {ok, RequestUrl, #{}}
            end;
        _ ->
            % ?LOG_INFO("method not support", []),
            {error, ?HTTP_ERROR, "method not support"}
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
                {ok, Method, RequestUrl, HttpVersion} ->
                    % 解析请求头数据：解析成映射诅
                    HeaderMap = getHeaderData(HeaderLines, #{}),

                    % 获取请求参数，区分 GET 和 POST 请求的参数位置
                    % ?LOG_INFO("~n~p~n~p~n~p~n", [Method, RequestUrl, RequestBody]),
                    case getRequestParams(Method, RequestUrl, RequestBody) of
                        {ok, RequestUrl1, ParamsMap} ->
                            % {ok, Method, #{"method" => Method, "url" => RequestUrl1, "httpVersion" => HttpVersion, "headerParams" => HeaderMap, "requestParams" => ParamsMap}};
                            {ok, Method, #request{method = Method, url = RequestUrl1, httpVersion = HttpVersion, headerParams = HeaderMap, requestParams = ParamsMap}};
                        {error, ErrorCode, Reason} ->
                            % 当前 http 请求方法不支持
                            {error, ErrorCode, Reason}
                    end;
                {error, ErrorCode, Reason} ->
                    % 解析请求行错误
                    {error, ErrorCode, Reason}
            end;
        {error, ErrorCode, Reason} ->
            % 从 套接字获取请求数据失败
            {error, ErrorCode, Reason}
    end.


response(Reponse) ->
    % B = iolist_to_binary(Str),
    % iolist_to_binary(
    %     io_lib:fwrite(
    %         "HTTP/1.0 200 Ok\nConnect-Type: text/html\nContent-length: ~p\n\n~s", [size(B), B]
    %     )
    % ).
    B = iolist_to_binary(Reponse#reponse.reponseData),
    % ?LOG_INFO("~p", [io_lib:fwrite("~p\n~p\n\n~s", [Reponse#reponse.reponseLine, string:join(Reponse#reponse.reponseHeader, "\n"), B])]),
    % Result = string:join(Reponse#reponse.reponseHeader, "\n"),
    Header = string:join(Reponse#reponse.reponseHeader, "\n"),
    ?LOG_INFO("~w", [Header]),
    ?LOG_INFO("~w", [Reponse#reponse.reponseLine]),
    ?LOG_INFO("~w", [io_lib:fwrite("~p\n~p\n\n~s", [Reponse#reponse.reponseLine, Header, B])]),
    % Result1 = string:join([Reponse#reponse.reponseLine, Result, "\n", B], "\n"),
    % ?LOG_INFO("~p", [Result1]),
    iolist_to_binary(
        io_lib:fwrite(
            "~p\n~p\n\n~s", [Reponse#reponse.reponseLine, Header, B]
        )
    ).

doSend(Socket, Msg) ->
    case gen_tcp:send(Socket, Msg) of
        ok ->
            ?LOG_INFO("reponse success", []),
            ok;
        {error, Reason} ->
            ?LOG_INFO("reponse fail, reason:~p", [Reason]),
            {error, ?TCP_ERROR, Reason}
    end.

doRecv(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            {ok, Data};
        {error, Reason} ->
            {error, ?TCP_ERROR, Reason}
    end.
