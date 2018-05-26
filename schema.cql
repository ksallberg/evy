CREATE KEYSPACE IF NOT EXISTS "evy"
  WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor': 3};

USE "evy";

CREATE TABLE IF NOT EXISTS "users" (
  "username" text PRIMARY KEY,
  "email" text,
  "encrypted_password" blob
);
