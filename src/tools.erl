-module(tools).
-author('zhifu liu<491836641@qq.com>').
-include("settings.hrl").
-export([
    format/2,
    printRequestMsg/1,
    getVariableType/1
]).

format(Fmt, Params) ->
  io:format(Fmt, Params).

printRequestMsg(Req) ->
    ?LOG_INFO("method:~p raw_path:~p path:~p parse_qs:~p parse_post:~p parse_cookie:~p", [Req:get(method), Req:get(raw_path), Req:get(path), Req:parse_qs(), Req:parse_post(), Req:parse_cookie()]).

% 获取变量类型
getVariableType(Value) ->
    % Data is_list:字符串也是数组
    if
        is_atom(Value) ->
            atom;
        is_binary(Value) ->
            binary;
        is_bitstring(Value) ->
            bitstring;
        is_boolean(Value) ->
            boolean;
        is_float(Value) ->
            float;
        is_function(Value) ->
            function;
        is_integer(Value) ->
            integer;
        is_list(Value) ->
            list;
        is_port(Value) ->
            port;
        is_reference(Value) ->
            reference;
        is_tuple(Value) ->
            tuple;
        true ->
            other
    end.
