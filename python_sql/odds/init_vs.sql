-- 玩家表
DROP TABLE IF EXISTS `version_control`;
CREATE TABLE IF NOT EXISTS `version_control` (
    v_version varchar(15) not null,
    PRIMARY KEY(v_version)
)DEFAULT CHARSET=utf8;
