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
