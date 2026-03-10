CREATE TABLE actors (
  id BIGSERIAL PRIMARY KEY,
  name TEXT NOT NULL COLLATE "identifier",
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE UNIQUE INDEX actors_name_index ON actors (name);
