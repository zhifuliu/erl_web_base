#!/usr/bin/env python
# -*- coding: utf-8 -*-

import MySQLdb

class BatchSql():
    cxn = None
    cur = None
    connectCfg = None
    def __init__(self, connectCfg):
        self.connectCfg = connectCfg
        self.cxn = MySQLdb.Connect(host = self.connectCfg['host'], user = self.connectCfg['user'], passwd = self.connectCfg['password'], db = self.connectCfg['db'], charset = "utf8", port = int(self.connectCfg['port']))
        #游标
        self.cur = self.cxn.cursor()

    def __del__(self):
        if self.cxn is not None:
            self.cxn.close()

    def execDict(self, dict, tableName):
        keyStr = ','.join(dict.keys())
        strFlag = ''
        for flag in dict.keys():
            if len(strFlag) == 0:
                strFlag = strFlag + '%s'
            else:
                strFlag = strFlag + ',%s'
        sql =  'insert into ' + tableName + ' ('+ keyStr + ')' + ' values(' + strFlag + ')'
        valList = []
        valList.append(tuple(dict.values()))
        self.cur.executemany(sql, valList)
        self.cxn.commit()

    def exeSqlUp(self, sql):
        result = self.cur.execute(sql)
        self.cxn.commit()
        return result

    def exeSqlFetch(self, sql):
        self.cur.execute(sql)
        return self.cur.fetchall()

    def fetchSql(self, sql):
        self.cur.execute(sql)
        self.cxn.commit()
        return self.cur.fetchall()
