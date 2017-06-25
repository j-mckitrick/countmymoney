-- Debit accounts

DROP TABLE IF EXISTS ASSET;
CREATE TABLE ASSET (
       idx int PRIMARY KEY AUTO_INCREMENT,
       user_idx int NOT NULL DEFAULT 0,
       entry_idx int NOT NULL DEFAULT 0,
       date date NOT NULL,
       category varchar(40) NOT NULL DEFAULT 'Deposit',
       description varchar(80) NOT NULL DEFAULT 'General',
       amount float NOT NULL DEFAULT 0
);

DROP TABLE IF EXISTS EXPENSE;
CREATE TABLE EXPENSE (
       idx int PRIMARY KEY AUTO_INCREMENT,
       user_idx int NOT NULL DEFAULT 0,
       entry_idx int NOT NULL DEFAULT 0,
       date date NOT NULL,
       category varchar(40) NOT NULL DEFAULT 'Deposit',
       description varchar(80) NOT NULL DEFAULT 'General',
       amount float NOT NULL DEFAULT 0
);

DROP TABLE IF EXISTS DRAW;
CREATE TABLE DRAW (
       idx int PRIMARY KEY AUTO_INCREMENT,
       user_idx int NOT NULL DEFAULT 0,
       entry_idx int NOT NULL DEFAULT 0,
       date date NOT NULL,
       category varchar(40) NOT NULL DEFAULT 'Deposit',
       description varchar(80) NOT NULL DEFAULT 'General',
       amount float NOT NULL DEFAULT 0
);

-- Credit accounts

DROP TABLE IF EXISTS LIABILITY;
CREATE TABLE LIABILITY (
       idx int PRIMARY KEY AUTO_INCREMENT,
       user_idx int NOT NULL DEFAULT 0,
       entry_idx int NOT NULL DEFAULT 0,
       date date NOT NULL,
       category varchar(40) NOT NULL DEFAULT 'Deposit',
       description varchar(80) NOT NULL DEFAULT 'General',
       amount float NOT NULL DEFAULT 0
);

DROP TABLE IF EXISTS EQUITY;
CREATE TABLE EQUITY (
       idx int PRIMARY KEY AUTO_INCREMENT,
       user_idx int NOT NULL DEFAULT 0,
       entry_idx int NOT NULL DEFAULT 0,
       date date NOT NULL,
       category varchar(40) NOT NULL DEFAULT 'Deposit',
       description varchar(80) NOT NULL DEFAULT 'General',
       amount float NOT NULL DEFAULT 0
);

DROP TABLE IF EXISTS REVENUE;
CREATE TABLE REVENUE (
       idx int PRIMARY KEY AUTO_INCREMENT,
       user_idx int NOT NULL DEFAULT 0,
       entry_idx int NOT NULL DEFAULT 0,
       date date NOT NULL,
       category varchar(40) NOT NULL DEFAULT 'Deposit',
       description varchar(80) NOT NULL DEFAULT 'General',
       amount float NOT NULL DEFAULT 0
);

