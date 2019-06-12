-- kristian@10:evy> \d
-- +----------+------------------+----------+----------+
-- | Schema   | Name             | Type     | Owner    |
-- |----------+------------------+----------+----------|
-- | public   | account          | table    | kristian |
-- | public   | entry            | table    | kristian |
-- | public   | portfolio        | table    | kristian |
-- | public   | portfolio_id_seq | sequence | kristian |
-- +----------+------------------+----------+----------+
-- SELECT 4
-- Time: 0.028s

CREATE DATABASE "evy";

USE evy;

CREATE TABLE Account (
  username varchar(10) NOT NULL,
  email varchar(50) NOT NULL,
  encrypted_password varchar(250) NOT NULL,
  PRIMARY KEY (username)
);

INSERT INTO Account (username, email, encrypted_password)
  VALUES ('kris', 'k@p.s', 'lol');

INSERT INTO Account (username, email, encrypted_password)
  VALUES ('elin', 'e@g.c', 'olo');

SELECT * FROM Account;

CREATE TABLE Portfolio (
  id SERIAL UNIQUE,
  name varchar(20) NOT NULL,
  owner varchar(20) NOT NULL,
  PRIMARY KEY (name, owner),
  FOREIGN KEY (owner) REFERENCES Account(username)
);

INSERT INTO Portfolio (name, owner) VALUES ('krisport1', 'kris');
INSERT INTO Portfolio (name, owner) VALUES ('krisport2', 'kris');
INSERT INTO Portfolio (name, owner) VALUES ('elinport1', 'elin');
INSERT INTO Portfolio (name, owner) VALUES ('elinport2', 'elin');

SELECT * FROM Portfolio;

CREATE TABLE Entry (
  portfolio_id int NOT NULL,
  symbol varchar(5) NOT NULL,
  type varchar(4) NOT NULL,
  units int NOT NULL,
  price float NOT NULL,
  ts timestamptz NOT NULL,
  PRIMARY KEY (portfolio_id, ts),
  FOREIGN KEY (portfolio_id) REFERENCES Portfolio(id)
);

INSERT INTO Entry (portfolio_id, symbol, type, units, price, ts)
  VALUES (1, 'msft', 'buy', 20, 30.3, '2019-04-20 12:00:00');

INSERT INTO Entry (portfolio_id, symbol, type, units, price, ts)
  VALUES (4, 'mdca', 'buy', 100, 2.4, '2019-06-07 13:12:00');

SELECT * FROM Entry;
