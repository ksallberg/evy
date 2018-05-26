CREATE KEYSPACE IF NOT EXISTS "evy"
  WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor': 3};

USE "evy";

-- username is the partitioning key, partitioning keys are mandatory.
-- there is no clustering column in users
CREATE TABLE IF NOT EXISTS "users" (
  "username" text PRIMARY KEY,
  "email" text,
  "encrypted_password" blob
);

-- _compound primary key_
-- username is partitioning key (mandatory)
-- id is a "clustering column".
-- The job of a clustering column is to determine the ordering
-- of rows within a partition.
--
-- CREATE TABLE "user_status_updates" (
--   "username" text,
--   "id" timeuuid,
--   "body" text,
--   PRIMARY KEY ("username", "id")
-- );
