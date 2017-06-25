
DROP SEQUENCE cmm_client_seq CASCADE;
CREATE SEQUENCE cmm_client_seq;

DROP TABLE cmm_client;
CREATE TABLE cmm_client (
       idx int PRIMARY KEY DEFAULT nextval('cmm_client_seq'),
       name varchar(30) NOT NULL default '',
       UNIQUE(name)
);

INSERT INTO cmm_client (name) VALUES ('CNA');
INSERT INTO cmm_client (name) VALUES ('Northside');
INSERT INTO cmm_client (name) VALUES ('Careflash');
INSERT INTO cmm_client (name) VALUES ('RLG');
