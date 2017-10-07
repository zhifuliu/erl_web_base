-module(visitors_dict).
-behaviour(gen_server).

-include("../settings.hrl").
% 本次服务器启动总接口调用次数，和最好一次调用时间
% 使用 dict 存储

% self apis
-export([start/0, stop/0]).

% gen_server apis : gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).       % 将 SERVER 宏设置为模块名
-define(VISITORS, "visitor_count").
-define(VISITORS_LAST_TIME, "visitor_last_time").

start() ->
    ?LOG_INFO("~p starting", [?SERVER]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

init([]) ->
    % 初始构建一个新的字典
    {ok, dict:new()}.

get(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, Count} ->
            ?LOG_INFO("~p : ~p", [Key, Count]),
            Count;
        error ->
            ?LOG_INFO("get ~p error", [Key]),
            error
    end.

handle_call({add}, _From, State) ->
    Count = get(?VISITORS, State),
    if
        Count == error ->
            NewState = dict:store(?VISITORS, 1, State),
            {reply, 1, NewState};
        true ->
            NewState = dict:store(?VISITORS, Count + 1, State),
            {reply, Count + 1, NewState}
    end;
handle_call({get}, _From, State) ->
    Count = get(?VISITORS, State),
    Lasttime = get(?VISITORS_LAST_TIME, State),
    {reply, {Count, Lasttime}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
