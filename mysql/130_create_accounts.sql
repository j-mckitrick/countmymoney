-- Debit accounts

DROP TABLE IF EXISTS asset;
CREATE TABLE asset (
       idx int PRIMARY KEY AUTO_INCREMENT,
       user_idx int NOT NULL DEFAULT 0,
       entry_idx int NOT NULL DEFAULT 0,
       date date NOT NULL,
       category varchar(40) NOT NULL DEFAULT 'Deposit',
       description varchar(80) NOT NULL DEFAULT 'General',
       amount float NOT NULL DEFAULT 0
);

DROP TABLE IF EXISTS expense;
CREATE TABLE expense (
       idx int PRIMARY KEY AUTO_INCREMENT,
       user_idx int NOT NULL DEFAULT 0,
       entry_idx int NOT NULL DEFAULT 0,
       date date NOT NULL,
       category varchar(40) NOT NULL DEFAULT 'Deposit',
       description varchar(80) NOT NULL DEFAULT 'General',
       amount float NOT NULL DEFAULT 0
);

DROP TABLE IF EXISTS draw;
CREATE TABLE draw (
       idx int PRIMARY KEY AUTO_INCREMENT,
       user_idx int NOT NULL DEFAULT 0,
       entry_idx int NOT NULL DEFAULT 0,
       date date NOT NULL,
       category varchar(40) NOT NULL DEFAULT 'Deposit',
       description varchar(80) NOT NULL DEFAULT 'General',
       amount float NOT NULL DEFAULT 0
);

-- Credit accounts

DROP TABLE IF EXISTS liability;
CREATE TABLE liability (
       idx int PRIMARY KEY AUTO_INCREMENT,
       user_idx int NOT NULL DEFAULT 0,
       entry_idx int NOT NULL DEFAULT 0,
       date date NOT NULL,
       category varchar(40) NOT NULL DEFAULT 'Deposit',
       description varchar(80) NOT NULL DEFAULT 'General',
       amount float NOT NULL DEFAULT 0
);

DROP TABLE IF EXISTS equity;
CREATE TABLE equity (
       idx int PRIMARY KEY AUTO_INCREMENT,
       user_idx int NOT NULL DEFAULT 0,
       entry_idx int NOT NULL DEFAULT 0,
       date date NOT NULL,
       category varchar(40) NOT NULL DEFAULT 'Deposit',
       description varchar(80) NOT NULL DEFAULT 'General',
       amount float NOT NULL DEFAULT 0
);

DROP TABLE IF EXISTS revenue;
CREATE TABLE revenue (
       idx int PRIMARY KEY AUTO_INCREMENT,
       user_idx int NOT NULL DEFAULT 0,
       entry_idx int NOT NULL DEFAULT 0,
       date date NOT NULL,
       category varchar(40) NOT NULL DEFAULT 'Deposit',
       description varchar(80) NOT NULL DEFAULT 'General',
       amount float NOT NULL DEFAULT 0
);

