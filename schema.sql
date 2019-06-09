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
  email varchar(20) NOT NULL,
  encrypted_password varchar(25) NOT NULL,
  PRIMARY KEY (username)
);

CREATE TABLE Portfolio (
  id SERIAL,
  name varchar(20) NOT NULL UNIQUE,
  owner varchar(20) NOT NULL,
  PRIMARY KEY (id),
  FOREIGN KEY (owner) REFERENCES Account(username)
);

CREATE TABLE Entry (
  portfolio_id int NOT NULL,
  symbol varchar(5) NOT NULL,
  type varchar(4) NOT NULL,
  units int NOT NULL,
  price float NOT NULL,
  ts timestamp NOT NULL,
  PRIMARY KEY (portfolio_id, ts),
  FOREIGN KEY (portfolio_id) REFERENCES Portfolio(id)
);
