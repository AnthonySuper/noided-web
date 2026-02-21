--
-- PostgreSQL database dump
--

\restrict N5IYXPjCSE2Uhg3hitr2mK1MxgZVNGArHkFZmfNqxhXQnhCzURYNcyvjcfWWDbw

-- Dumped from database version 17.6 (Postgres.app)
-- Dumped by pg_dump version 18.2

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET transaction_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: case_insensitive; Type: COLLATION; Schema: public; Owner: -
--

CREATE COLLATION public.case_insensitive (provider = icu, deterministic = false, locale = 'und');


--
-- Name: identifier; Type: COLLATION; Schema: public; Owner: -
--

CREATE COLLATION public.identifier (provider = icu, deterministic = false, locale = 'und-u-ks-level1');


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.schema_migrations (
    filename text NOT NULL
);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (filename);


--
-- PostgreSQL database dump complete
--

\unrestrict N5IYXPjCSE2Uhg3hitr2mK1MxgZVNGArHkFZmfNqxhXQnhCzURYNcyvjcfWWDbw

--
-- PostgreSQL database dump
--

\restrict XNU53SWdeEHVWfV5LKHCMJd2SKLOuc35W4Yu30cETxvALMUT3ciyPuN1khtFY6A

-- Dumped from database version 17.6 (Postgres.app)
-- Dumped by pg_dump version 18.2

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET transaction_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Data for Name: schema_migrations; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO public.schema_migrations (filename) VALUES
	('202602210012_create_case_insensitive_collation.rb'),
	('202602210049_create_identifier_collation.rb');


--
-- PostgreSQL database dump complete
--

\unrestrict XNU53SWdeEHVWfV5LKHCMJd2SKLOuc35W4Yu30cETxvALMUT3ciyPuN1khtFY6A

