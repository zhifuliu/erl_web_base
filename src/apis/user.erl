-module(user).
-include("../settings.hrl").

-export([login/2, register/2, getUserInfo/2]).

login(HeaderLines, Params) ->
    ?LOG_INFO("api:login ; ~p ; ~p", [HeaderLines, Params]),
    ok.

register(HeaderLines, Params) ->
    ?LOG_INFO("api:register ; ~p ; ~p", [HeaderLines, Params]),
    ok.

getUserInfo(HeaderLines, Params) ->
    ?LOG_INFO("api:getUserInfo ; ~p ; ~p", [HeaderLines, Params]),
    ok.
