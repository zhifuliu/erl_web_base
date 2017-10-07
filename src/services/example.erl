-module(example).
-behaviour(gen_server).

% self apis
-export([start/0, stop/0, add/2, find/1]).
% gen_server apis : gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).       % 将 SERVER 宏设置为模块名

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

add(Key, Value) ->
    gen_server:call(?SERVER, {add, Key, Value}).

find(Key) ->
    gan_server:call(?SERVER, {find, Key}).

init([]) ->
    {ok, dict:new()}.

handle_call({add, Key, Value}, _From, Dict) ->
    Reply = dict:store(Key, Value, Dict),
    {reply, ok, Reply};
handle_call({find, Key}, _From, Dict) ->
    Reply = dict:find(Key, Dict),
    {reply, Reply, Dict}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
