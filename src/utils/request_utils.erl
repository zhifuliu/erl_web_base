-module(request_utils).

-include("../settings.hrl").

-export([apiFilter/2, listAllInMap/3, generateReponse/5, generateReturn/4]).

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
    % ReponseLine1 = string:join([HttpVersion, StatusCode, Msg], " "),
    % ?LOG_INFO("~p", [ReponseLine1]),
    % ReponseLine = io_lib:format("~w ~w ~w", [HttpVersion, StatusCode, Msg]),
    % ?LOG_INFO("~p", [erlang:list_to_atom(ReponseLine)]),
    % ReponseLine = string:join([HttpVersion, StatusCode, Msg], " "),
    % ReponseLine = io_lib:fwrite("~p ~p ~p", [erlang:list_to_integer(HttpVersion), erlang:list_to_integer(StatusCode), erlang:list_to_integer(Msg)]),
    ?LOG_INFO("~p", [tools:getVariableType(size(B))]),
    ?LOG_INFO("~p", [tools:getVariableType(StatusCode)]),
    ?LOG_INFO("~p", [tools:getVariableType(erlang:list_to_integer(StatusCode))]),
    ReponseLine = io_lib:fwrite("HTTP/~w ~w ~w", [unicode:characters_to_list(HttpVersion), unicode:characters_to_list(StatusCode), unicode:characters_to_list(Msg)]),
    % ReponseHeader = [
    %     "Content-type: application/json",
    %     string:join(["Content-length: ", erlang:integer_to_list(erlang:size(B))], "")
    % ],
    ReponseHeader = io_lib:fwrite("Content-type: application/json\nContent-length: ~p", [size(B)]),

    % string:join(["Content-length: ", erlang:integer_to_list(erlang:size(B))], "")
    HeaderParams,
    #reponse{reponseLine=ReponseLine, reponseHeader=ReponseHeader, reponseData=ReponseData}.

% encode(Message, _EntryPoint) ->
%
% encode(Term, EntryPoint) ->
%     jsx_encoder:encode(Term, EntryPoint).

% 构造返回数据
generateReturn(Result, Code, Reason, Data) ->
    ReturnData = #returnData{result=Result, code=Code, message=Reason, data=Data},
    ?LOG_INFO("~p", [ReturnData]),
    ReturnData.
