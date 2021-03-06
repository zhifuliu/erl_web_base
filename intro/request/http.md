参考：<http://www.jianshu.com/p/80e25cb1d81a>

# 请求消息 Request

客户端发送一个 http 请求到服务器的请求消息包括以下格式，换行符为：\r\n

- 请求行: 第一行
- 请求头: 空行之前除请求行的数据行
- 空行
- 请求数据: 比如 post 请求的参数会放在这里面

## 解释

1. 请求行: 以一个方法名开头，以控股分开，后面跟着请求的 url 和协议的版本（路径和协议版本也是以空格分开）
2. 请求头: 用来说明服务器要使用的附加信息
3. 空行: 请求头后面的空行是必须的，即使第四部分的数据为空，也必须有空行
4. 主体: 请求数据，可以添加任意数据

# 响应消息 Reponse

一般情况下，服务器接收并处理客户端发过来的请求后会返回一个 HTTP 的响应消息。HTTP 响应也由四部分组成，分别是:

- 状态行
- 消息报头
- 空行
- 响应正文

## 解释

1. 状态行: 由 HTTP 协议版本号、状态码、详细状态 三部分组成
2. 纤细报头: 用来说明客户端要使用的一些附加信息（Date:响应生成的时间日期；COntent-Type: MIME类型和编码类型）
3. 空行
4. 响应正文，服务器返回给客户端的文本消息。

## 不同开头状态码意义

code | remark
---- | ----------------------
1xx  | 指示信息--表示请求已接受，继续处理
2xx  | 成功--表示请求已被成功接受、理解、处理
3xx  | 重定向--要完成请求必须尽心更进一步的操作
4xx  | 客户端错误--请求有语法错误或者请求无法实现
5xx  | 服务端错误--服务器未能实现合法的请求

## 常见状态码

code | mean                  | remark
---- | --------------------- | ---------------------------------------
200  | ok                    | 请求成功
400  | bad request           | 请求有语法错误，不能被服务器理解
401  | Unauthorized          | 请求未经授权，这个状态代码必须和WWW-Authenticate报头域一起使用
403  | forbidden             | 收到请求，拒绝服务
404  | not found             | 资源不存在，url错误
500  | internal server error | 服务器发生错误
503  | server unavailabel    | 服务器不能处理客户端的请求，一段时间后可能恢复

## http 请求方法

method  | remark
------- | ------------------------------
GET     | 请求指定的页面信息，并返回实体主体
HEAD    | 类似get请求，只不过返回的响应中没有具体内容，用于获取报头
POST    | 想指定资源提交数据进行处理，数据被包含在请求体中
PUT     | 从客户端向服务器传送的数据取代指定的文档的内容
DELETE  | 请求服务器删除指定的页面
CONNECT | http1.1中预留给能够将连接改成管道方式的代理服务器
OPTIONS | 允许客户端查看服务器的性能
TRACE   | 回显服务器收到的请求，主要用于测试或诊断

# http 工作原理

http协议定义web客户端如何从web服务器请求web页面，以及服务器如何把web页面传送给客户端。http协议采用了请求、响应模型。客户端向服务器发送一个请求报文，请求报文包含请求的方法、url、协议版本、请求头和请求数据。服务器以一个状态行作为响应，响应的内容包括协议的版本、成功或者错误代码、服务器信息、响应头部和响应数据。

HTTP 请求/响应的步骤：

1. 客户端连接到web服务器。通过套接字连接
2. 发送http请求。通过套接字发送
3. 服务器接受请求并返回 http 响应
4. 释放 tcp 连接
5. 客户端浏览器解析 html 内容并运行

## GET 和 POST 请求的区别

## 应答头信息

参考：<http://www.cnblogs.com/mumue/archive/2012/04/23/2467072.html>

key           | remark
------------- | -------------------------------
Cache-Control | 指定请求和响应遵循的缓存机制
Date          | 消息发送时间
Pragma        | 用来包含特定的指令，最常用的是 Parama:no-cache

## 典型的响应消息：

```
HTTP/1.0200OK
Date:Mon,31Dec200104:25:57GMT
Server:Apache/1.3.14(Unix)
Content-type:text/html
Last-modified:Tue,17Apr200106:46:28GMT
Etag:"a030f020ac7c01:1e9f"
Content-length:39725426
Content-range:bytes554554-40279979/40279980
```

分别表示：<br>

```
1
```
