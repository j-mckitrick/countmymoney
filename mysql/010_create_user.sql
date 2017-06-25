
DROP TABLE IF EXISTS CMM_USER;
CREATE TABLE CMM_USER (
       idx int PRIMARY KEY AUTO_INCREMENT,
       username varchar(30) NOT NULL default '',
       password varchar(30) NOT NULL default '',
       admin boolean not null default false,
       UNIQUE(username)
);

INSERT INTO CMM_USER (username, password) VALUES ('jcm', 'jcm');
INSERT INTO CMM_USER (username, password) VALUES ('demo', 'demo');

