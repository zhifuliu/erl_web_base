-module(request_utils).

-include("../settings.hrl").

-export([apiFilter/2, listAllInMap/3]).

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
    Url = maps:get("url", Req),
    Params = maps:get("requestParams", Req),
    % ?LOG_INFO("~p", [?ApiList]),
    ?LOG_INFO("~p ; ~p", [Url, Params]),
    case maps:find(Url, ?ApiList) of
        {ok, Value} ->
            {{requireLogin, RequireLogin}, {params, RequireParams}} = Value,
            ?LOG_INFO("~p ; ~p ; ~p", [Url, RequireLogin, RequireParams]),
            case (RequireLogin =:= true) and (LogindIn =:= false) of
                true ->
                    % 要登录却没有登录
                    {error, string:join(["api:", Url, " require login"], "")};
                false ->
                    % 不需要登录或者已经登录，那么检查参数
                    case listAllInMap(true, RequireParams, Params) of
                        true ->
                            % 需要的参数都上传了
                            ok;
                        false ->
                            % 参数不完整
                            {error, string:join(["api:", Url, " param lost. required parmas:", RequireParams], "")}
                    end
            end;
        error ->
            {error, "api not exist"}
    end.
