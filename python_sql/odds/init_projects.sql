-- 所有工程配置表
-- DROP TABLE IF EXISTS `projects`;
CREATE TABLE IF NOT EXISTS `projects` (
    i_project_id int not null,
    v_region varchar(20) not null,
    v_svr_name varchar(20) not null,
    v_config varchar(256) not null,
    PRIMARY KEY(i_project_id)
)DEFAULT CHARSET=utf8;
