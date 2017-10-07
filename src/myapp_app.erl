-module(myapp_app).

-behaviour(application).

-include("settings.hrl").

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    start(type, args).

start(_StartType, _StartArgs) ->
    % myapp_sup:start_link(),
    % og4erl:conf("priv/log4erl.conf"),
    myapp_web:start(8080, "../src/test"),
    ?LOG("~p", ["myapp_app start"]),
    myapp_sup:start_link().

stop(_State) ->
    ok.
