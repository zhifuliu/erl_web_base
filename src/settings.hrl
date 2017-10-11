% 接口访问 logger
-define(ACCESS_LOG(Format, Args), log4erl:info(access_logger, "module:~p line:~p " ++ Format, [?MODULE,?LINE]++Args)).

% 程序逻辑 logger
-define(LOG_INFO(Format, Args), log4erl:info("module:~p line:~p " ++ Format, [?MODULE,?LINE]++Args)).
-define(LOG_WARN(Format, Args), log4erl:warn("module:~p line:~p " ++ Format, [?MODULE,?LINE]++Args)).
-define(LOG_ERROR(Format, Args), log4erl:error("module:~p line:~p " ++ Format, [?MODULE,?LINE]++Args)).
-define(LOG_FATAL(Format, Args), log4erl:fatal("module:~p line:~p " ++ Format, [?MODULE,?LINE]++Args)). % fatal 就是打印在 console 里面，不写进文件

-define(LOG(Format, Args), io:format("module:~p line:~p " ++ Format ++ "~n", [?MODULE,?LINE]++Args)).

% 所有 api 列表,配置信息：名字、是否需要登录、参数列表
-define(ApiList,
    #{
        "userInfo_get" => {{requireLogin, true}, {params, []}},
        "register_get" => {{requireLogin, false}, {params, ["account", "password"]}},
        "login_get" => {{requireLogin, false}, {params, ["account", "password"]}},
        "userInfo_post" => {{requireLogin, true}, {params, []}},
        "register_post" => {{requireLogin, false}, {params, ["account", "password"]}},
        "login_post" => {{requireLogin, false}, {params, ["account", "password"]}}
    }
).
