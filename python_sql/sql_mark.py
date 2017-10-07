#!/usr/bin/env python
# -*- coding: utf-8 -*-

# 读取本目录下 sql 文件夹的所有 sql 文件名，通过时间来排序，读取数据库最后一个版本的版本号，并执行没有执行过的版本号标示的 sql 文件
import sys
import os
from sys import argv
import utils.get_config as configUtils
from utils import BatchSql
from collections import OrderedDict

# 本工具的所有功能：初始化、自动执行到最新版本

def printHelp():
    print("command list:")
    print("python3 sql_mark.py init svrName. 初始化具体工程，从工程表格中拉取本工程数据库配置，数据库并创建一个对应的数据库账号")
    print("python3 sql_mark.py up svrName [version]. 将指定工程的数据库个更新到指定版本，如果 version 没有指定，那么更新的到最新版本")
    exit()

def initSvr():
    global regionName, svrName
    rootMySQLCfg = configUtils.rootMySQLCfg
    svrMySQLCfg = configUtils.getSvrConfig(regionName, svrName)
    if svrMySQLCfg is not None:
        svrMySQLCfg = svrMySQLCfg['cfg']
        os.system('./utils/init_project.sh %s %s %s %s %s %s' % (rootMySQLCfg['host'], rootMySQLCfg['user'], rootMySQLCfg['password'], svrMySQLCfg['user'], svrMySQLCfg['password'], svrMySQLCfg['db']))
    else:
        print("获取 svr 信息失败，请确认projects 表格中配置了该服务器")
        exit()
    print("%s 数据库和用户创建完成")

def up():
    global sqlDict, executedVersion, batchSql, regionName, svrName, sqlFileDir
    svrMySQLCfg = configUtils.getSvrConfig(regionName, svrName)
    if svrMySQLCfg is None:
        print("获取 svr 信息失败，请确认projects 表格中配置了该服务器")
        exit()
    svrMySQLCfg = svrMySQLCfg['cfg']
    batchSql = BatchSql(svrMySQLCfg)

    # 解析出所有 sql 文件版本号
    getSqlFileDict()
    # print(sqlDict)

    # 获取已执行的版本号列表
    getExecutedVersionList()

    # 获取未执行版本号列表并排序
    unExwcuteVersion = {}
    for version in sqlDict:
        if version not in executedVersion:
            unExwcuteVersion[version] = sqlDict[version]

    for version in OrderedDict(sorted(unExwcuteVersion.items(), key = lambda t:t[0])):
        if (upVersion is None) or (upVersion is not None and int(version) < int(upVersion)):
            execResult = os.system("./utils/execute_sql.sh %s %s %s %s %s" % (svrMySQLCfg['host'], svrMySQLCfg['user'], svrMySQLCfg['password'], svrMySQLCfg['db'], sqlFileDir + sqlDict[version]))
            if execResult == 0:
                writeInVersionTable(version)
            else:
                print("执行版本号：" + str(version) + " 失败")
                exit()
    print("数据库已更新到最新版本")

def getSqlFileDict():
    global sqlDict, sqlFileDir
    allSqlFileNale = os.listdir(sqlFileDir)
    for filename in allSqlFileNale:
        temp = filename.split(".")[0].split("_")
        sqlVersion = temp[len(temp)-1:len(temp)][0]
        if sqlVersion not in sqlDict:
            sqlDict[sqlVersion] = filename

# 获取当前所有执行过的版本号
def getExecutedVersionList():
    global executedVersion, batchSql
    result = batchSql.fetchSql("select v_version from version_control")
    for item in result:
        version = item[0]
        if len(version) > 0:
            executedVersion.append(version)

# 判断一个版本号文件是否执行
def hadExecuted(checkVersion):
    global executedVersion
    return checkVersion in executedVersion

# 执行完一个版本号后，版本号写进数据库
def writeInVersionTable(writeVersion):
    global executedVersion, batchSql
    executedVersion.append(writeVersion)
    batchSql.exeSqlUp("insert into version_control values('" + str(writeVersion) + "')")


if __name__ == "__main__":
    argvLength = len(sys.argv)
    if argvLength < 2:
        printHelp()

    sqlFileDir = "./version_files/"
    sqlDict = {}
    # 当前已执行过的版本号
    executedVersion = []
    command = sys.argv[1]
    regionName = None
    svrName = None
    upVersion = None

    svrMysqlCfg = None

    if command == "help":
        printHelp()
    elif command == "init":
        if argvLength != 4:
            print("init example: python3 sql_mark.py init regionName svrName")
            exit()
        else:
            regionName = sys.argv[2]
            svrName = sys.argv[3]
            initSvr()
    elif command == "up":
        if argvLength not in [4, 5]:
            print("up example: python3 sql_mark.py up regionName svrName [version]")
            exit()
        else:
            regionName = sys.argv[2]
            svrName = sys.argv[3]
            if argvLength == 5:
                upVersion = sys.argv[4]
            up()
    else:
        printHelp()
