% 接口访问 logger
-define(ACCESS_LOG(Format, Args), log4erl:info(access_logger, "module:~p line:~p " ++ Format, [?MODULE,?LINE]++Args)).

% 程序逻辑 logger
-define(LOG_INFO(Format, Args), log4erl:info("module:~p line:~p " ++ Format, [?MODULE,?LINE]++Args)).
-define(LOG_WARN(Format, Args), log4erl:warn("module:~p line:~p " ++ Format, [?MODULE,?LINE]++Args)).
-define(LOG_ERROR(Format, Args), log4erl:error("module:~p line:~p " ++ Format, [?MODULE,?LINE]++Args)).
-define(LOG_FATAL(Format, Args), log4erl:fatal("module:~p line:~p " ++ Format, [?MODULE,?LINE]++Args)). % fatal 就是打印在 console 里面，不写进文件

-define(LOG(Format, Args), io:format("module:~p line:~p " ++ Format ++ "~n", [?MODULE,?LINE]++Args)).
