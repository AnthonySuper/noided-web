CREATE COLLATION public.case_insensitive (provider = icu, deterministic = false, locale = 'und-u-ks-level2');
CREATE COLLATION public.identifier (provider = icu, deterministic = false, locale = 'und-u-ks-level1');
CREATE TYPE public.organization_access_level AS ENUM (
    'guest',
    'member',
    'admin'
);
CREATE TYPE public.unit AS ENUM (
    'gram',
    'kilogram',
    'ounce',
    'pound',
    'milliliter',
    'liter',
    'hectoliter',
    'fluid_ounce',
    'gallon',
    'us_beer_barrel',
    'each',
    'minute',
    'hour'
);
CREATE TYPE public.unit_category AS ENUM (
    'mass',
    'volume',
    'time',
    'count'
);
CREATE FUNCTION public.to_canonical_qty(qty numeric, u public.unit) RETURNS numeric
    LANGUAGE sql IMMUTABLE STRICT
    AS $$
  SELECT (CASE u
    WHEN 'gram' THEN qty
    WHEN 'kilogram' THEN qty * 1000
    WHEN 'ounce' THEN qty * 28.3495
    WHEN 'pound' THEN qty * 453.592
    WHEN 'milliliter' THEN qty
    WHEN 'liter' THEN qty * 1000
    WHEN 'hectoliter' THEN qty * 100000
    WHEN 'fluid_ounce' THEN qty * 29.5735
    WHEN 'gallon' THEN qty * 3785.41
    WHEN 'us_beer_barrel' THEN qty * 117348
    WHEN 'each' THEN qty
    WHEN 'minute' THEN qty
    WHEN 'hour' THEN qty * 60
  END)::NUMERIC(20,6);
$$;
COMMENT ON FUNCTION public.to_canonical_qty(qty numeric, u public.unit) IS 'converts a quantity in some unit-of-measure to its equivalent quantity in the base unit (gram, ml, each, minute)';
CREATE FUNCTION public.to_unit_category(u public.unit) RETURNS public.unit_category
    LANGUAGE sql IMMUTABLE STRICT
    AS $$
  SELECT CASE u
    WHEN 'gram' THEN 'mass'::unit_category
    WHEN 'kilogram' THEN 'mass'::unit_category
    WHEN 'ounce' THEN 'mass'::unit_category
    WHEN 'pound' THEN 'mass'::unit_category
    WHEN 'milliliter' THEN 'volume'::unit_category
    WHEN 'liter' THEN 'volume'::unit_category
    WHEN 'hectoliter' THEN 'volume'::unit_category
    WHEN 'fluid_ounce' THEN 'volume'::unit_category
    WHEN 'gallon' THEN 'volume'::unit_category
    WHEN 'us_beer_barrel' THEN 'volume'::unit_category
    WHEN 'each' THEN 'count'::unit_category
    WHEN 'minute' THEN 'time'::unit_category
    WHEN 'hour' THEN 'time'::unit_category
  END;
$$;
COMMENT ON FUNCTION public.to_unit_category(u public.unit) IS 'returns the category of a given unit';
CREATE TABLE public.actors (
    id bigint NOT NULL,
    name text NOT NULL COLLATE public.identifier,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
CREATE SEQUENCE public.actors_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.actors_id_seq OWNED BY public.actors.id;
CREATE TABLE public.items (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    name text NOT NULL COLLATE public.identifier,
    description text DEFAULT ''::text NOT NULL,
    default_unit public.unit NOT NULL,
    measure_category public.unit_category GENERATED ALWAYS AS (public.to_unit_category(default_unit)) STORED NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
CREATE SEQUENCE public.items_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.items_id_seq OWNED BY public.items.id;
CREATE TABLE public.login_attempts (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    user_agent text,
    remote_ip inet NOT NULL,
    attempt_at timestamp with time zone NOT NULL,
    successful boolean NOT NULL
);
CREATE SEQUENCE public.login_attempts_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.login_attempts_id_seq OWNED BY public.login_attempts.id;
CREATE TABLE public.organization_user_accesses (
    organization_id bigint NOT NULL,
    user_id bigint NOT NULL,
    access_level public.organization_access_level NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
CREATE TABLE public.organizations (
    id bigint NOT NULL,
    name text NOT NULL COLLATE public.identifier,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
CREATE SEQUENCE public.organizations_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.organizations_id_seq OWNED BY public.organizations.id;
CREATE TABLE public.schema_migrations (
    filename text NOT NULL
);
CREATE TABLE public.sessions (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    user_agent text,
    remote_ip inet NOT NULL,
    valid_during tstzrange NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
CREATE SEQUENCE public.sessions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.sessions_id_seq OWNED BY public.sessions.id;
CREATE TABLE public.user_default_organizations (
    user_id bigint NOT NULL,
    organization_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
CREATE TABLE public.user_passwords (
    user_id bigint NOT NULL,
    password_digest text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
CREATE TABLE public.users (
    id bigint NOT NULL,
    email text NOT NULL COLLATE public.case_insensitive,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
ALTER TABLE ONLY public.actors ALTER COLUMN id SET DEFAULT nextval('public.actors_id_seq'::regclass);
ALTER TABLE ONLY public.items ALTER COLUMN id SET DEFAULT nextval('public.items_id_seq'::regclass);
ALTER TABLE ONLY public.login_attempts ALTER COLUMN id SET DEFAULT nextval('public.login_attempts_id_seq'::regclass);
ALTER TABLE ONLY public.organizations ALTER COLUMN id SET DEFAULT nextval('public.organizations_id_seq'::regclass);
ALTER TABLE ONLY public.sessions ALTER COLUMN id SET DEFAULT nextval('public.sessions_id_seq'::regclass);
ALTER TABLE ONLY public.actors
    ADD CONSTRAINT actors_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.items
    ADD CONSTRAINT items_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.login_attempts
    ADD CONSTRAINT login_attempts_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.organization_user_accesses
    ADD CONSTRAINT organization_user_accesses_pkey PRIMARY KEY (organization_id, user_id);
ALTER TABLE ONLY public.organizations
    ADD CONSTRAINT organizations_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (filename);
ALTER TABLE ONLY public.sessions
    ADD CONSTRAINT sessions_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.user_default_organizations
    ADD CONSTRAINT user_default_organizations_pkey PRIMARY KEY (user_id);
ALTER TABLE ONLY public.user_passwords
    ADD CONSTRAINT user_passwords_pkey PRIMARY KEY (user_id);
ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);
CREATE UNIQUE INDEX actors_name_index ON public.actors USING btree (name);
CREATE INDEX login_attempts_remote_ip_attempt_at_index ON public.login_attempts USING btree (remote_ip, attempt_at);
CREATE INDEX login_attempts_user_id_attempt_at_index ON public.login_attempts USING btree (user_id, attempt_at);
CREATE INDEX organization_user_accesses_user_id_index ON public.organization_user_accesses USING btree (user_id);
CREATE UNIQUE INDEX organizations_name_index ON public.organizations USING btree (name);
CREATE INDEX sessions_user_id_index ON public.sessions USING btree (user_id);
CREATE INDEX sessions_valid_during_index ON public.sessions USING gist (valid_during);
CREATE INDEX user_default_organizations_organization_id_index ON public.user_default_organizations USING btree (organization_id);
CREATE UNIQUE INDEX users_email_index ON public.users USING btree (email);
ALTER TABLE ONLY public.items
    ADD CONSTRAINT items_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.organizations(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.login_attempts
    ADD CONSTRAINT login_attempts_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.organization_user_accesses
    ADD CONSTRAINT organization_user_accesses_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.organizations(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.organization_user_accesses
    ADD CONSTRAINT organization_user_accesses_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.sessions
    ADD CONSTRAINT sessions_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.user_default_organizations
    ADD CONSTRAINT user_default_organizations_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.organizations(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.user_default_organizations
    ADD CONSTRAINT user_default_organizations_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.user_passwords
    ADD CONSTRAINT user_passwords_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_id_fkey FOREIGN KEY (id) REFERENCES public.actors(id) ON DELETE CASCADE;

-- Applied migrations
INSERT INTO schema_migrations (filename) VALUES ('202602210012_create_case_insensitive_collation');
INSERT INTO schema_migrations (filename) VALUES ('202602210049_create_identifier_collation');
INSERT INTO schema_migrations (filename) VALUES ('202602210052_create_actors_table');
INSERT INTO schema_migrations (filename) VALUES ('202602210201_create_users_table');
INSERT INTO schema_migrations (filename) VALUES ('202602210202_create_user_passwords_table');
INSERT INTO schema_migrations (filename) VALUES ('202602220255_create_login_attempts_table');
INSERT INTO schema_migrations (filename) VALUES ('202602220302_create_sessions_table');
INSERT INTO schema_migrations (filename) VALUES ('202602221947_create_organization_tables');
INSERT INTO schema_migrations (filename) VALUES ('202602240135_create_unit_category_enum');
INSERT INTO schema_migrations (filename) VALUES ('202602240137_create_unit_enum');
INSERT INTO schema_migrations (filename) VALUES ('202602240144_create_canonicalize_unit_function');
INSERT INTO schema_migrations (filename) VALUES ('202602240158_create_items_table');
