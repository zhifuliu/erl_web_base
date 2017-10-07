-- 用户表
DROP TABLE IF EXISTS `user`;
CREATE TABLE IF NOT EXISTS `user` (
    v_sid varchar(30) not null,
    i_type_code smallint UNSIGNED not null default 1,
    d_created_at datetime not null,
    d_updated_at datetime not null,
    b_removed boolean default false,

    i_display_id int UNSIGNED not null not null,
    v_email varchar(128) default "",
    i_phone_number int UNSIGNED not null,
    v_passward varchar(128) not null,
    v_user_name varchar(30) not null,
    v_avatar varchar(128) not null
)DEFAULT CHARSET=utf8;
