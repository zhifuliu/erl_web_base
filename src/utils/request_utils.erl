-module(request_utils).

-include("../settings.hrl").

-export([apiFilter/2, listAllInMap/3, generateReponse/5]).

% 本模块用来处理请求，比如：接口过滤器

% 判断一个列表的所有项是否都在一个映射组中
listAllInMap(true, List, Map) when erlang:length(List) > 0 ->
    % ?LOG_INFO("~p ; ~p", [List, Map]),
    [H | O] = List,
    case maps:find(H, Map) of
        {ok, _} ->
            listAllInMap(true, O, Map);
        error ->
            listAllInMap(false, O, Map)
    end;
listAllInMap(true, [], _) ->
    true;
listAllInMap(false, _, _) ->
    false.

% 接口过滤
apiFilter(Req, LogindIn) ->
    % Url = maps:get("url", Req),
    % Params = maps:get("requestParams", Req),
    Url = Req#request.url,
    Params = Req#request.requestParams,
    % ?LOG_INFO("~p", [?ApiList]),
    % ?LOG_INFO("~p ; ~p", [Url, Params]),
    case maps:find(Url, ?ApiList) of
        {ok, Value} ->
            {{requireLogin, RequireLogin}, {params, RequireParams}, {handler, Module, Func}} = Value,
            % ?LOG_INFO("~p ; ~p ; ~p", [Url, RequireLogin, RequireParams]),
            case (RequireLogin =:= true) and (LogindIn =:= false) of
                true ->
                    % 要登录却没有登录
                    {error, ?NOT_LOGIN, string:join(["api:", Url, " require login"], "")};
                false ->
                    % 不需要登录或者已经登录，那么检查参数
                    case listAllInMap(true, RequireParams, Params) of
                        true ->
                            % 需要的参数都上传了
                            {ok, Module, Func};
                        false ->
                            % 参数不完整
                            {error, ?PARAMS_ERROR, string:join(["api:", Url, " param lost. required parmas:", RequireParams], "")}
                    end
            end;
        error ->
            {error, ?NOT_FOUND, "api not exist"}
    end.

% 通过给定数据，构造一个 reponse 结构。只有简单的信息：Content-type、Content-length
generateReponse(StatusCode, Msg, HttpVersion, HeaderParams, ReponseData) ->
    B = iolist_to_binary(ReponseData),
    ReponseLine = string:join([HttpVersion, StatusCode, Msg], " "),
    ReponseHeader = [
        "Content-type:application/json",
        string:join(["Content-length", erlang:size(B)], ":")
    ],
    HeaderParams,
    #reponse{reponseLine=ReponseLine, reponseHeader=ReponseHeader, reponseData=ReponseData}.
