
DROP SEQUENCE entry_seq CASCADE;
CREATE SEQUENCE entry_seq;

DROP TABLE entry;
CREATE TABLE entry (
       idx int PRIMARY KEY DEFAULT nextval('entry_seq'),
       user_idx int NOT NULL DEFAULT 0,
       date date NOT NULL DEFAULT now(),
       category varchar(40) NOT NULL DEFAULT 'Deposit',
       description varchar(80) NOT NULL DEFAULT 'General',
       amount float NOT NULL DEFAULT 0
);

--INSERT INTO entry (user_idx, category, amount) VALUES (1, 'EXPENSE', 100);
--INSERT INTO entry (user_idx, category, amount) VALUES (1, 'OWNERDRAW', 20);
