
DROP TABLE IF EXISTS cmm_client;
CREATE TABLE cmm_client (
       idx int PRIMARY KEY AUTO_INCREMENT,
       name varchar(30) NOT NULL default '',
       UNIQUE(name)
);

INSERT INTO cmm_client (name) VALUES ('CNA');
INSERT INTO cmm_client (name) VALUES ('Careflash');
INSERT INTO cmm_client (name) VALUES ('RLG');
INSERT INTO cmm_client (name) VALUES ('Northside');
INSERT INTO cmm_client (name) VALUES ('SNTS');
