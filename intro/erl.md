# 介绍 erl 命令的使用，这样能够写出好的 shell 启动脚本

## 参考：

- <http://blog.csdn.net/mycwq/article/details/12610677>
- <http://blog.csdn.net/summerhust/article/details/8698993>

## 注意：

1 之前一直使用 shell 脚本运行出错，因为 erl 命令的 -s 参数，指定的不是 app 文件的名字，而是 behaviour 指定为 application 的 erl 文件的名字，指定 -s 文件后，会调用 application:start("-s name")，而且没有参数传入的时候，需要 export 一个 start/0 的函数
