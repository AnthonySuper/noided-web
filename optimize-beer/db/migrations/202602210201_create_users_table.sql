CREATE TABLE users (
  id BIGINT PRIMARY KEY REFERENCES actors(id) ON DELETE CASCADE,
  email TEXT NOT NULL COLLATE "case_insensitive",
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE UNIQUE INDEX users_email_index ON users (email);
