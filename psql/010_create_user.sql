
DROP SEQUENCE cmm_user_seq CASCADE;
CREATE SEQUENCE cmm_user_seq;

DROP TABLE cmm_user;
CREATE TABLE cmm_user (
       idx int PRIMARY KEY DEFAULT nextval('cmm_user_seq'),
       username varchar(30) NOT NULL default '',
       password varchar(30) NOT NULL default '',
       admin boolean not null default false,
       UNIQUE(username)
);

INSERT INTO cmm_user (username, password) VALUES ('jcm', 'jcm');
INSERT INTO cmm_user (username, password) VALUES ('demo', 'demo');
