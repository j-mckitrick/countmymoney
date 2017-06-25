-- Debit accounts

DROP SEQUENCE asset_seq CASCADE;
CREATE SEQUENCE asset_seq;

DROP TABLE asset;
CREATE TABLE asset (
       idx int PRIMARY KEY DEFAULT nextval('asset_seq'),
       user_idx int NOT NULL DEFAULT 0,
       entry_idx int NOT NULL DEFAULT 0,
       date date NOT NULL DEFAULT now(),
       category varchar(40) NOT NULL DEFAULT 'Deposit',
       description varchar(80) NOT NULL DEFAULT 'General',
       amount float NOT NULL DEFAULT 0
);

DROP SEQUENCE expense_seq CASCADE;
CREATE SEQUENCE expense_seq;

DROP TABLE expense;
CREATE TABLE expense (
       idx int PRIMARY KEY DEFAULT nextval('expense_seq'),
       user_idx int NOT NULL DEFAULT 0,
       entry_idx int NOT NULL DEFAULT 0,
       date date NOT NULL DEFAULT now(),
       category varchar(40) NOT NULL DEFAULT 'Deposit',
       description varchar(80) NOT NULL DEFAULT 'General',
       amount float NOT NULL DEFAULT 0
);

DROP SEQUENCE equity_seq CASCADE;
CREATE SEQUENCE equity_seq;

DROP TABLE equity;
CREATE TABLE equity (
       idx int PRIMARY KEY DEFAULT nextval('equity_seq'),
       user_idx int NOT NULL DEFAULT 0,
       entry_idx int NOT NULL DEFAULT 0,
       date date NOT NULL DEFAULT now(),
       category varchar(40) NOT NULL DEFAULT 'Deposit',
       description varchar(80) NOT NULL DEFAULT 'General',
       amount float NOT NULL DEFAULT 0
);

-- Credit accounts

DROP SEQUENCE liability_seq CASCADE;
CREATE SEQUENCE liability_seq;

DROP TABLE liability;
CREATE TABLE liability (
       idx int PRIMARY KEY DEFAULT nextval('liability_seq'),
       user_idx int NOT NULL DEFAULT 0,
       entry_idx int NOT NULL DEFAULT 0,
       date date NOT NULL DEFAULT now(),
       category varchar(40) NOT NULL DEFAULT 'Deposit',
       description varchar(80) NOT NULL DEFAULT 'General',
       amount float NOT NULL DEFAULT 0
);

DROP SEQUENCE equity_seq CASCADE;
CREATE SEQUENCE equity_seq;

DROP TABLE equity;
CREATE TABLE equity (
       idx int PRIMARY KEY DEFAULT nextval('equity_seq'),
       user_idx int NOT NULL DEFAULT 0,
       entry_idx int NOT NULL DEFAULT 0,
       date date NOT NULL DEFAULT now(),
       category varchar(40) NOT NULL DEFAULT 'Deposit',
       description varchar(80) NOT NULL DEFAULT 'General',
       amount float NOT NULL DEFAULT 0
);

DROP SEQUENCE revenue_seq CASCADE;
CREATE SEQUENCE revenue_seq;

DROP TABLE revenue;
CREATE TABLE revenue (
       idx int PRIMARY KEY DEFAULT nextval('revenue_seq'),
       user_idx int NOT NULL DEFAULT 0,
       entry_idx int NOT NULL DEFAULT 0,
       date date NOT NULL DEFAULT now(),
       category varchar(40) NOT NULL DEFAULT 'Deposit',
       description varchar(80) NOT NULL DEFAULT 'General',
       amount float NOT NULL DEFAULT 0
);

