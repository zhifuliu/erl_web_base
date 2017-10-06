-module(myapp_app).

-behaviour(application).

-include("settings.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    og4erl:conf("priv/log4erl.conf"),
    myapp_sup:start_link().

stop(_State) ->
    ok.
