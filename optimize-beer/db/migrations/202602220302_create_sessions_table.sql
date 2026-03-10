CREATE TABLE sessions (
  id BIGSERIAL PRIMARY KEY,
  user_id BIGINT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  user_agent TEXT,
  remote_ip INET NOT NULL,
  valid_during TSTZRANGE NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX sessions_user_id_index ON sessions (user_id);
CREATE INDEX sessions_valid_during_index ON sessions USING GIST (valid_during);
