CREATE TABLE login_attempts (
  id BIGSERIAL PRIMARY KEY,
  user_id BIGINT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  user_agent TEXT,
  remote_ip INET NOT NULL,
  attempt_at TIMESTAMPTZ NOT NULL,
  successful BOOLEAN NOT NULL
);

CREATE INDEX login_attempts_user_id_attempt_at_index ON login_attempts (user_id, attempt_at);
CREATE INDEX login_attempts_remote_ip_attempt_at_index ON login_attempts (remote_ip, attempt_at);
