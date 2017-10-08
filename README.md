# 关于本工程

erlang 编写的 web 应用基础，包括：解析请求成 Request 结构；提供一个数据库服务，服务是 gen_server 形式，数据库只有用户表，用于验证登录、注册、获取玩家信息接口

# 关于 swagger ui

swagger ui 是一个接口文档工具，书写 json 文件就可以实现。使用方法：

1. 克隆 <https://github.com/zhifuliu/swagger-Ui> 到本地
2. 将本工程的 swagger.json 文件拷贝到 swagger-ui 工程的 public 目录
3. 安装依赖: npm install --verbose
4. 安装 gulp: sudo npm install -g gulp --verbose
5. 运行工程。在 swagger-ui 工程的跟目录运行命令: gulp

此时会在浏览器打开一个页面，如果修改 swagger-ui 的代码，页面会实时刷新
