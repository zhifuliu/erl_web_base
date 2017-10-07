-module(tools).
-author('zhifu liu<491836641@qq.com>').
-include("settings.hrl").
-export([
    format/2
    % printRequestMsg/1
]).

format(Fmt, Params) ->
  io:format(Fmt, Params).

% printRequestMsg(Req) ->
%     ?LOG_INFO("method:~p raw_path:~p path:~p parse_qs:~p parse_post:~p parse_cookie:~p", [Req:get(method), Req:get(raw_path), Req:get(path), Req:parse_qs(), Req:parse_post(), Req:parse_cookie()]).
