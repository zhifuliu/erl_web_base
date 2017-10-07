# gen_server

erlang 程序设计里面有一个原则就是把进程构造成树，把公用代码提出来，特定的懂你用自己的 modules 实现，也就是 behaviour，应用 behaviour 可以减少于本身事务无关的代码了，设计逻辑更加清晰。

客户端服务器模型,通用 server 框架，用于多个客户端公用一个资源的情况（比如数据库中的数据读写，只能有一个入口来操作，很适合做成 gen_server 的形式），由几个接口函数和几个回调函数组成。gen_server 的接口有六个函数:

- init/1
- handle_call/3
- handle_cast
- handle_info/2
- terminate/2

流程:

1. gen_server:start_link 的调用会生成一个服务器进程且连接到进程树。
2. gen_server:call/2 的调用导致对 handle_call 的调用，这是同步的
3. gen_server:cast/2 的调用导致多 handle_cast 的调用，这是一步的

gen_server的停止规则：

1. 以 gen_server:start_link 开始的连入监控树的：一般情况不需要提供自己的停止函数，监控进程会自动处理，但是如果想在 gen_server 进程中自己清理以下资源，那么必须在 init 函数里调用 process_flag(trap_exit, true) 来捕获推出信号，这会导致调用 terminate/2 函数，所以必须实现这个函数
2. 以gen_server:start 开始的单独 gen_server: 直接调用 gen_server:cast(Name, stop), 这会导致调用 handle_cast/2

---------- 分割线 ----------

参考：<http://diaocow.iteye.com/blog/1756615>

编写一个 gen_server 的 callback 模块步骤：

1. 给 calback Module 命名
2. 确定需要向外界（客户端）提供的接口
3. 实现 gen_server 所要求的回调接口

/src/services/example.erl 实现了一个 KV 存储,下面是对代码的解释：

- behaviour(gen_server): 表示让编译器检查，当前 module 是否实现了 gen_server 指定的所有回调接口
- gen_server:start_link(ServerName, Module, Args, Options) -> Result: 这个方法用来启动一个 server，参数依次是：服务名、该 server 的callback 模块、服务的初始化启动参数（服务初始化时会调用：Module:init([Args])、特性参数。如果服务启动成功，返回 {ok, Pid}
- Module:init([Args]): 在服务初始化时被调用，如果初始化成功，返回 {ok, State}，其中 State 将作为启动服务的 State
- gen_server:call(ServerRef, Request): 调用 ServerRef 表示的服务的 handle_call，handle_call 是同步调用，会一直等待服务器返回以恶搞响应消息（除非等待超时，默认5s)
- Module:handle_call(Request, From, State) -> Result:Module 模块的 handle_call 回调函数，当有地方调用 Module 模块的 handle_call 时，会出发这个方法，参数依次是 客户端请求（调用本服务的地方可以指定要调用了方法，然后 Module:handle_call 通过模式匹配来调用具体方法来处理客户端的请求）、来自哪个客户端、当前服务器状态。
- gen_server:cast(ServerRef, Request):调用一步方法
- Module:handle_cast: 处理异步调用

result 类型：

以 reply 开头，那么 Reply 将会作为响应返回给客户端；以 noreply 开头，那么服务器将不会返回任何消息给客户端（这个导致客户端阻塞，因为客户端调用的 gen_server:call 是一个同步方法，一定要等服务端返回数据

result                           | 解释
-------------------------------- | ---
{reply,Reply,NewState}           | 有返回
{reply,Reply,NewState,Timeout}   | 有返回
{reply,Reply,NewState,hibernate} | 有返回
{noreply,NewState}               | 无返回
{noreply,NewState,Timeout}       | 无返回
{noreply,NewState,hibernate}     | 无返回
{stop,Reason,Reply,NewState}     | 有返回
{stop,Reason,NewState}           | 有返回

---------- 分割线 ----------

参考：<http://blog.csdn.net/lqg1122/article/details/7484413>

gen_server 提供了 C/S 架构中的服务端的实现，即定义了自己一套规范的服务器框架。

gen_server 实现流程：

1. 先定义模块的行为模式为 gen_server：-behaviour(gen_server)
2. 实现 gen_server 的回调方法，并 export 出来
3. export 模块的对外调用函数
4. 实现2和3 export 出来的函数
5. 调用 gen_server:start 或者 gen_server:start_link 启动 server
6. 在注册名字成功后，新的 gen_server 进程会调用回调函数 Module:init([Args])，init 返回 {ok, State}，其中 State 是 gen_server 的内部状态

对于 start_link，调用形式：gen_server:start_link(ServerName, Module, Args, Options)，参数的具体意义如下。这里要注意：

1. gen_server:start_link 是同步的。只有等到 gen_server 被完全初始化并准备接受请求之后才返回
2. 如果 gen_server 是某棵监督树的一部分，即 gen_server 是由一个督程启动的，那么必须使用 gen_server:start_link。还有另外一个函数 gen_server:start 用于启动一个独立的 gen_server。

3. ServerName: server 的名字，别的进程可以通过这个名字调用该 server 暴露的接口

4. Module: 回调模块的名字，也就是回调函数放在哪个模块里面。一般来说，将代表同一个进程的代码和同一个模块放在一起

5. Args: 将原封不动传递给回调函数 init

6. Options: 参数列表

---------- 分割线 ----------

## 总结

流程如上面这个一样，将在 myapp_sup.erl 中注册 gen_server
