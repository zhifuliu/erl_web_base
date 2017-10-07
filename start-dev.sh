#!/bin/sh

if [ ! -d logs ]; then
  mkdir logs
fi

# 用来将所有的第三方依赖添加进code路径，也可以单个引入 如：-pa 3rd/mochiweb/ebin ； pa 3rd/log4erl/ebin
# -pa 3rd/*/ebin \

exec erl \
    -pa ebin \
    -pa 3rd/*/ebin \
    -boot start_sasl \
    -sname myapp_dev \
    -s myapp_app \
    -s reloader \
    # -detached

    # -pa 将目录加到搜索路径的开头，-pz 则将目录加到搜索路径的末尾。用于加载 code，指定 code 目录
    # -boot 指定启动文件 start_sasl.boot
    # -s appName 同 -run Mod Func 用脚本来动态启动 app,
    # -name 节点名字
