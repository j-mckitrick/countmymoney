
DROP TABLE IF EXISTS cmm_user;
CREATE TABLE cmm_user (
       idx int PRIMARY KEY AUTO_INCREMENT,
       username varchar(30) NOT NULL default '',
       password varchar(30) NOT NULL default '',
       admin boolean not null default false,
       UNIQUE(username)
);

INSERT INTO cmm_user (username, password) VALUES ('jcm', 'jcm');
INSERT INTO cmm_user (username, password) VALUES ('demo', 'demo');

