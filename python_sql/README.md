# 目录介绍

本目录作用是数据库的管理工具

# 脚本作用

- deploy_cfg.ini: 发布配置文件
- create_db.sh: 用来创建数据库并创建该数据库的用户
- build_db.sh: 初始化第一版本的数据库，执行 lust.sql
- get_deploy_cfg.[py|sh]: 从远程数据库获取要发布环境的数据库配置，用来代替 deploy_cfg.ini，用文件配置有很大的局限性
- lust.sql: 数据库文件。lust.sql 为第一版，后面有修改的话加版本号
- update_db.sh: 执行相应版本的 sql 文件
- patch.sh: 检查数据库版本之间的差异

# 流程

1 create_db.sh

2 build_db.sh

3 update_db.sh
