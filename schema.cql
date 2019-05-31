CREATE KEYSPACE IF NOT EXISTS "evy"
  WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor': 3};

USE "evy";

-- _compound primary key_
-- username is partitioning key (mandatory)
-- encrypted_password is clustring column
--
-- The job of a clustering column is to determine the ordering
-- of rows within a partition.
--
--
-- without password as part of compound primary key, following is not possible:
-- select * from users where username='bob' and encrypted_password='veoa';
--
-- Cannot execute this query as it might involve data filtering and thus may
-- have unpredictable performance. If you want to execute this query despite
-- the performance unpredictability, use ALLOW FILTERING
CREATE TABLE IF NOT EXISTS "users" (
  "username" text,
  "email" text,
  "encrypted_password" text,
  PRIMARY KEY ("username", "encrypted_password")
);
-- INSERT INTO users (username, email, encrypted_password) VALUES ('bob', 'a@a.se', 'pass');
-- SELECT * FROM users;

CREATE TABLE IF NOT EXISTS "portfolios" (
  "name" text,
  "owner" text, -- point to users.username
  "id" uuid,
  PRIMARY KEY ("name", "owner")
);

-- index needed to be able to write "where owner='xxx'"
CREATE INDEX ON "portfolios" (owner);

-- INSERT INTO portfolios (name, owner, id) VALUES ('svenska', 'apa', NOW());
-- SELECT * FROM portfolios;

CREATE TABLE IF NOT EXISTS "entry" (
  "id" uuid,
  "portfolio_id" uuid,
  "symbol" text,
  "type" text, -- sell or buy
  "units" int,
  "price" FLOAT,
  "when" timestamp,
  PRIMARY KEY ("id")
);

CREATE INDEX ON "entry" (portfolio_id);

-- INSERT INTO entry (id, portfolio_id, symbol, type, units, price) VALUES (NOW(), NOW(), 'DPS', 'buy', 1, 232.2);
-- SELECT * FROM entry;
