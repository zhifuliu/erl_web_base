-module(dict_test).

-export([add/2, get/1]).

get(Key) ->
    Dict = dict:new(),
    case dict:find(Key, Dict) of
        {ok, Value} ->
            io:format("~p : ~p", [Key, Value]);
        error ->
            io:format("get ~p error", [Key])
    end,
    ok.

add(Key, Value) ->
    Dict = dict:new(),
    dict:store(Key, Value, Dict),
    ok.
