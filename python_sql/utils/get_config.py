#!/usr/bin/env python
# -*- coding: utf-8 -*-

from .batch_sql import BatchSql
import json

rootMySQLCfg = {
    "host": "localhost",
    "user": "root",
    "password": "123456",
    "port": 3306,
    "db": "projects_cfg",
    "table": "projects"
}
batchSql = BatchSql(rootMySQLCfg)

# 获取指定地区的所有数据库配置
def getRegionSvrList(regionName):
    global rootMySQLCfg, batchSql
    result = {}
    selectSql = "select * from " + rootMySQLCfg['table'] + " where v_region='" + regionName + "'"
    sqlResult = batchSql.fetchSql(selectSql)
    for item in sqlResult:
        svrname = item[2]
        if svrname not in result:
            result[svrname] = {
                "region": item[1],
                "projectId": item[0],
                "cfg": json.loads(item[3].replace("'", "\""))
            }
    return result

def getSvrConfig(regionName, svrName):
    regionSvrList = getRegionSvrList(regionName)
    for item in regionSvrList:
        if svrName == item:
            return regionSvrList[item]
    print("未找到配置，region: %s, svrName: %s" % (regionName, svrName))
    return None
