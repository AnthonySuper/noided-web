CREATE TABLE items (
  id BIGSERIAL PRIMARY KEY,
  organization_id BIGINT NOT NULL REFERENCES organizations(id) ON DELETE CASCADE,
  name TEXT NOT NULL COLLATE "identifier",
  description TEXT NOT NULL DEFAULT '',
  default_unit unit NOT NULL,
  measure_category unit_category NOT NULL 
    GENERATED ALWAYS AS (to_unit_category(default_unit)) STORED,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
