--
-- PostgreSQL database dump
--

\restrict 2xWbGtvMiETH1TpTLFhSkEQVbwtx46DGYiZIxnRKMovjqxt5SEsSjQUeFd8b4H7

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

\unrestrict 2xWbGtvMiETH1TpTLFhSkEQVbwtx46DGYiZIxnRKMovjqxt5SEsSjQUeFd8b4H7

--
-- PostgreSQL database dump
--

\restrict 5Okj9YOL6emrlTtmYdOKx2M6Lbxa5jXkhpveCr7sKXaSDOBTgXxSuzKtOZPmwxx

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
	('202602210012_create_case_insensitive_collation.rb');


--
-- PostgreSQL database dump complete
--

\unrestrict 5Okj9YOL6emrlTtmYdOKx2M6Lbxa5jXkhpveCr7sKXaSDOBTgXxSuzKtOZPmwxx

