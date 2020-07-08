CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE SCHEMA payment;

-- Data

CREATE TYPE payment.correspondent_type_t AS
ENUM (
    -- Account holder is located in the country of the currency
    'CORRESPONDENT',
    -- Account holder is not located in the country of the currency
    -- Follow the SSI chain
    'LOCAL_CORRESPONDENT'
);

CREATE TYPE payment.routing_ssi_source_type AS
ENUM ('PAGOFX', 'SWIFT');

CREATE TYPE payment.holiday_type_type AS
ENUM ('NORMAL_HOLIDAY', 'SPECIAL_HOLIDAY', 'EXCHANGE_HOLIDAY', 'WEEKEND_HOLIDAY');


CREATE TYPE payment.beneficiary_institution_reachability_type AS
ENUM ('DIRECT', 'INDIRECT');

CREATE TABLE payment.iban_structure (
    iban_structure_id uuid NOT NULL
        DEFAULT gen_random_uuid(),
    iban_country_code varchar(2) NOT NULL,
    iban_country_code_position integer NOT NULL,
    iban_country_code_length integer NOT NULL,
    iban_check_digits_position integer NOT NULL,
    iban_check_digits_length integer NOT NULL,
    bank_identifier_position integer NOT NULL,
    bank_identifier_length integer NOT NULL,
    branch_identifier_position integer,
    branch_identifier_length integer,
    iban_national_id_length integer NOT NULL,
    account_number_position integer NOT NULL,
    account_number_length integer NOT NULL,
    iban_total_length integer NOT NULL,
    optional_commence_date date,
    mandatory_commence_date date,
    is_sepa bool NOT NULL
        DEFAULT false,
    creation_ts timestamptz NOT NULL
        DEFAULT date_trunc('milliseconds', current_timestamp),
    update_ts timestamptz NOT NULL
        DEFAULT date_trunc('milliseconds', current_timestamp),
    CONSTRAINT pk_iban_structure
        PRIMARY KEY (iban_structure_id),
    CONSTRAINT uq_iban_structure_iban_country_code
        UNIQUE (iban_country_code)
);

CREATE TABLE payment.iban_institution (
    iban_institution_id uuid NOT NULL
        DEFAULT gen_random_uuid(),
    -- Instituiton name of the IBAN issuer
    institution_name varchar(105) NOT NULL,
    institution_country_name varchar(70) NOT NULL,
    institution_country_code varchar(2) NOT NULL,
    -- Beneficiary institution in SEPA payments
    -- otherwise use SEPAROUTING.INTERMEDIARY_INSTITUTION_BIC
    iban_bic varchar(11) NOT NULL,
    -- Is not related to SEPA payments
    -- SWIFT-connected correspondent of an unconnected bank
    -- Central payment processing bank with many unconnected branches
    routing_bic varchar(11) NOT NULL,
    -- Sufficient to derive correctly BIC from an IBAN
    iban_national_id varchar(15) NOT NULL,
    -- In some cases may differ from insitution_country_code
    iban_country_code varchar(2) NOT NULL,
    -- False means that institution uses the IBAN standrad
    -- but is not a participant of SEPA
    is_sepa bool NOT NULL
        DEFAULT false,
    creation_ts timestamptz NOT NULL
        DEFAULT date_trunc('milliseconds', current_timestamp),
    update_ts timestamptz NOT NULL
        DEFAULT date_trunc('milliseconds', current_timestamp),
    CONSTRAINT pk_iban_institution
        PRIMARY KEY (iban_institution_id),
    CONSTRAINT uq_iban_institution_iban_bic
        UNIQUE (iban_bic),
    CONSTRAINT uq_iban_institution_iban_national_id
        UNIQUE (iban_national_id),
    CONSTRAINT fk_iban_has_iban_structure
        FOREIGN KEY (iban_country_code)
        REFERENCES payment.iban_structure (iban_country_code)
        ON UPDATE RESTRICT ON DELETE RESTRICT
);

CREATE TABLE payment.iban_exclusion_list (
    iban_exclusion_list_id uuid NOT NULL
        DEFAULT gen_random_uuid(),
    iban_country_code varchar(2) NOT NULL,
    iban_national_id varchar(15) NOT NULL,
    iban_bic varchar(11),
    iban_invalid_date date,
    creation_ts timestamptz NOT NULL
        DEFAULT date_trunc('milliseconds', current_timestamp),
    update_ts timestamptz NOT NULL
        DEFAULT date_trunc('milliseconds', current_timestamp),
    CONSTRAINT pk_iban_exclusion_list
        PRIMARY KEY (iban_exclusion_list_id)
);

CREATE TABLE payment.bank_holiday (
    bank_holiday_id uuid NOT NULL
        DEFAULT gen_random_uuid(),
    country_code varchar(2) NOT NULL,
    holiday_date date NOT NULL,
    holiday_type payment.holiday_type_type NOT NULL,
    holiday_details jsonb,
    holiday_services varchar(3)[],
    creation_ts timestamptz NOT NULL
        DEFAULT date_trunc('milliseconds', current_timestamp),
    update_ts timestamptz NOT NULL
        DEFAULT date_trunc('milliseconds', current_timestamp),
    CONSTRAINT pk_bank_holiday
        PRIMARY KEY (bank_holiday_id),
    CONSTRAINT uq_bank_holiday_country_code_holiday_date_holiday_type
        UNIQUE (country_code, holiday_date, holiday_type)
);

CREATE TABLE payment.sepa_routing (
    sepa_routing_id uuid NOT NULL
        DEFAULT gen_random_uuid(),
    beneficiary_bic varchar(11) NOT NULL,
    beneficiary_institution_name varchar(105) NOT NULL,
    beneficiary_institution_country_code varchar(2) NOT NULL,
    beneficiary_institution_city varchar(35) NOT NULL,
    sepa_scheme varchar(8) NOT NULL,
    payment_channel varchar(11) NOT NULL,
    is_preferred_channel bool NOT NULL
        DEFAULT false,
    beneficiary_institution_reachability
        payment.beneficiary_institution_reachability_type
        DEFAULT 'DIRECT',
    intermediaryBic varchar(11),
    creation_ts timestamptz NOT NULL
        DEFAULT date_trunc('milliseconds', current_timestamp),
    update_ts timestamptz NOT NULL
        DEFAULT date_trunc('milliseconds', current_timestamp),
    CONSTRAINT pk_sepa_routing
        PRIMARY KEY (sepa_routing_id),
    CONSTRAINT uq_sepq_routing_beneficiary_bic_country_scheme_channel
        UNIQUE (beneficiary_bic, beneficiary_institution_country_code,
            sepa_scheme, payment_channel)
);

CREATE TABLE payment.routing_ssi (
    routing_ssi_id uuid NOT NULL
        DEFAULT gen_random_uuid(),
    beneficiary_bic varchar(11) NOT NULL,
    beneficiary_institution_name varchar(105) NOT NULL,
    beneficiary_institution_city varchar(35) NOT NULL,
    beneficiary_institution_country_code varchar(2) NOT NULL,
    currency_code varchar(3) NOT NULL,
    asset_category varchar(4) NOT NULL,
    -- BIC Account Holding Institution
    -- Final correspondent or intermediary correspondent
    -- BIC where the beneficiaryBic holds the account
    -- lookup localCorrespondent as beneficiaryBic to follow the SSI chain
    -- beneficiaryBic has no direct correspondent relationship in the country of the currency,
    -- but works with a local correspondent as an intermediary
    correspondent_bic varchar(11) NOT NULL,
    correspondent_institution_name varchar(105) NOT NULL,
    correspondent_country_code varchar(2) NOT NULL,
    correspondent_type payment.correspondent_type_t NOT NULL,
    routing_ssi_source payment.routing_ssi_source_type NOT NULL,
    beneficiary_account_number_in_correspondent_bic varchar(70),
    is_preferred_correspondent bool
        DEFAULT false,
    start_date date,
    end_date date,
    creation_ts timestamptz NOT NULL
        DEFAULT date_trunc('milliseconds', current_timestamp),
    update_ts timestamptz NOT NULL
        DEFAULT date_trunc('milliseconds', current_timestamp),
    CONSTRAINT pk_routing_ssi
        PRIMARY KEY (routing_ssi_id),
    CONSTRAINT uq_routing_ssi_bic_currency_country_ssi_source
        UNIQUE (beneficiary_bic, currency_code, beneficiary_institution_country_code,
            correspondent_bic, routing_ssi_source)
);

-- Interface

CREATE OR REPLACE FUNCTION payment.put_iban_structure (
    a_iban_country_code varchar(2),
    a_iban_country_code_position integer,
    a_iban_country_code_length integer,
    a_iban_check_digits_position integer,
    a_iban_check_digits_length integer,
    a_bank_identifier_position integer,
    a_bank_identifier_length integer,
    a_iban_national_id_length integer,
    a_account_number_position integer,
    a_account_number_length integer,
    a_iban_total_length integer,
    a_branch_identifier_position integer DEFAULT NULL,
    a_branch_identifier_length integer DEFAULT NULL,
    a_optional_commence_date date DEFAULT NULL,
    a_mandatory_commence_date date DEFAULT NULL,
    a_is_sepa bool DEFAULT false
) RETURNS uuid
LANGUAGE sql AS $$
INSERT INTO payment.iban_structure (
    iban_country_code,
    iban_country_code_position,
    iban_country_code_length,
    iban_check_digits_position,
    iban_check_digits_length,
    bank_identifier_position,
    bank_identifier_length,
    branch_identifier_position,
    branch_identifier_length,
    iban_national_id_length,
    account_number_position,
    account_number_length,
    iban_total_length,
    optional_commence_date,
    mandatory_commence_date,
    is_sepa
) VALUES (
    a_iban_country_code,
    a_iban_country_code_position,
    a_iban_country_code_length,
    a_iban_check_digits_position,
    a_iban_check_digits_length,
    a_bank_identifier_position,
    a_bank_identifier_length,
    a_branch_identifier_position,
    a_branch_identifier_length,
    a_iban_national_id_length,
    a_account_number_position,
    a_account_number_length,
    a_iban_total_length,
    a_optional_commence_date,
    a_mandatory_commence_date,
    a_is_sepa
) ON CONFLICT ON CONSTRAINT uq_iban_structure_iban_country_code DO UPDATE SET
    iban_country_code_position = excluded.iban_country_code_position,
    iban_country_code_length = excluded.iban_country_code_length,
    iban_check_digits_position = excluded.iban_check_digits_position,
    iban_check_digits_length = excluded.iban_check_digits_length,
    bank_identifier_position = excluded.bank_identifier_position,
    bank_identifier_length = excluded.bank_identifier_length,
    branch_identifier_position = excluded.branch_identifier_position,
    branch_identifier_length = excluded.branch_identifier_length,
    iban_national_id_length = excluded.iban_national_id_length,
    account_number_position = excluded.account_number_position,
    account_number_length = excluded.account_number_length,
    iban_total_length = excluded.iban_total_length,
    optional_commence_date = excluded.optional_commence_date,
    mandatory_commence_date = excluded.mandatory_commence_date,
    is_sepa = excluded.is_sepa,
    update_ts = date_trunc('milliseconds', current_timestamp)
RETURNING iban_structure_id;
$$;

CREATE OR REPLACE FUNCTION payment.put_iban_institution (
    a_institution_name varchar(105),
    a_institution_country_name varchar(70),
    a_institution_country_code varchar(2),
    a_iban_bic varchar(11),
    a_routing_bic varchar(11),
    a_iban_national_id varchar(15),
    a_iban_country_code varchar(2),
    a_is_sepa bool DEFAULT false
) RETURNS uuid
LANGUAGE sql AS $$
INSERT INTO payment.iban_institution (
    institution_name,
    institution_country_name,
    institution_country_code,
    iban_bic,
    routing_bic,
    iban_national_id,
    iban_country_code,
    is_sepa
 ) VALUES (
    a_institution_name,
    a_institution_country_name,
    a_institution_country_code,
    a_iban_bic,
    a_routing_bic,
    a_iban_national_id,
    a_iban_country_code,
    a_is_sepa
) ON CONFLICT ON CONSTRAINT uq_iban_institution_iban_bic DO UPDATE SET
    institution_name = excluded.institution_name,
    institution_country_name = excluded.institution_country_name,
    institution_country_code = excluded.institution_country_code,
    routing_bic = excluded.routing_bic,
    iban_national_id = excluded.iban_national_id,
    iban_country_code = excluded.iban_country_code,
    is_sepa = excluded.is_sepa,
    update_ts = date_trunc('milliseconds', current_timestamp)
RETURNING iban_institution_id;
$$;

CREATE OR REPLACE FUNCTION payment.put_iban_exclusion_list (
    a_iban_country_code varchar(2),
    a_iban_national_id varchar(15),
    a_iban_bic varchar(11) DEFAUlT NULL,
    a_iban_invalid_date date DEFAUlT NULL
) RETURNS uuid
LANGUAGE sql AS $$
INSERT INTO payment.iban_exclusion_list (
    iban_country_code,
    iban_national_id,
    iban_bic,
    iban_invalid_date
 ) VALUES (
    a_iban_country_code,
    a_iban_national_id,
    a_iban_bic,
    a_iban_invalid_date
) ON CONFLICT ON CONSTRAINT pk_iban_exclusion_list DO UPDATE SET
    iban_country_code = excluded.iban_country_code,
    iban_national_id = excluded.iban_national_id,
    iban_bic = excluded.iban_bic,
    iban_invalid_date = excluded.iban_invalid_date,
    update_ts = date_trunc('milliseconds', current_timestamp)
RETURNING iban_exclusion_list_id;
$$;

CREATE OR REPLACE FUNCTION payment.put_bank_holiday(
    a_country_code varchar(2),
    a_holiday_date date,
    a_holiday_type payment.holiday_type_type
) RETURNS uuid
LANGUAGE sql AS $$
INSERT INTO payment.bank_holiday (country_code, holiday_date, holiday_type)
VALUES (a_country_code, a_holiday_date, a_holiday_type)
ON CONFLICT ON CONSTRAINT uq_bank_holiday_country_code_holiday_date_holiday_type
DO UPDATE SET
    country_code = excluded.country_code,
    holiday_date = excluded.holiday_date,
    holiday_type = excluded.holiday_type,
    update_ts = date_trunc('milliseconds', current_timestamp)
RETURNING bank_holiday_id;
$$;

CREATE OR REPLACE FUNCTION payment.put_sepa_routing (
    a_beneficiary_bic varchar(11),
    a_beneficiary_institution_name varchar(105),
    a_beneficiary_institution_country_code varchar(2),
    a_beneficiary_institution_city varchar(35),
    a_sepa_scheme varchar(8),
    a_payment_channel varchar(11),
    a_is_preferred_channel bool DEFAULT false,
    a_beneficiary_institution_reachability
        payment.beneficiary_institution_reachability_type DEFAULT 'DIRECT',
    a_intermediaryBic varchar(11) DEFAULT NULL
) RETURNS uuid
LANGUAGE sql AS $$
INSERT INTO payment.sepa_routing (
    beneficiary_bic,
    beneficiary_institution_name,
    beneficiary_institution_country_code,
    beneficiary_institution_city,
    sepa_scheme,
    payment_channel,
    is_preferred_channel,
    beneficiary_institution_reachability,
    intermediaryBic
) VALUES (
    a_beneficiary_bic,
    a_beneficiary_institution_name,
    a_beneficiary_institution_country_code,
    a_beneficiary_institution_city,
    a_sepa_scheme,
    a_payment_channel,
    a_is_preferred_channel,
    a_beneficiary_institution_reachability,
    a_intermediaryBic
) ON CONFLICT ON CONSTRAINT uq_sepq_routing_beneficiary_bic_country_scheme_channel
DO UPDATE SET
    beneficiary_institution_name = excluded.beneficiary_institution_name,
    beneficiary_institution_city = excluded.beneficiary_institution_city,
    is_preferred_channel = excluded.is_preferred_channel,
    beneficiary_institution_reachability = excluded.beneficiary_institution_reachability,
    intermediaryBic = excluded.intermediaryBic,
    update_ts = date_trunc('milliseconds', current_timestamp)
RETURNING sepa_routing_id;
$$;

CREATE OR REPLACE FUNCTION payment.put_routing_ssi (
    a_beneficiary_bic varchar(11),
    a_beneficiary_institution_name varchar(105),
    a_beneficiary_institution_city varchar(35),
    a_beneficiary_institution_country_code varchar(2),
    a_currency_code varchar(3),
    a_asset_category varchar(4),
    a_correspondent_bic varchar(11),
    a_correspondent_institution_name varchar(105),
    a_correspondent_country_code varchar(2),
    a_correspondent_type payment.correspondent_type_t,
    a_routing_ssi_source payment.routing_ssi_source_type,
    a_beneficiary_account_number_in_correspondent_bic varchar(70) DEFAULT NULL,
    a_is_preferred_correspondent bool DEFAULT false,
    a_start_date date DEFAULT NULL,
    a_end_date date DEFAULT NULL
) RETURNS uuid
LANGUAGE sql AS $$
INSERT INTO payment.routing_ssi (
    beneficiary_bic,
    beneficiary_institution_name,
    beneficiary_institution_city,
    beneficiary_institution_country_code,
    currency_code,
    asset_category,
    correspondent_bic,
    correspondent_institution_name,
    correspondent_country_code,
    correspondent_type,
    routing_ssi_source,
    beneficiary_account_number_in_correspondent_bic,
    is_preferred_correspondent,
    start_date,
    end_date
 ) VALUES (
    a_beneficiary_bic,
    a_beneficiary_institution_name,
    a_beneficiary_institution_city,
    a_beneficiary_institution_country_code,
    a_currency_code,
    a_asset_category,
    a_correspondent_bic,
    a_correspondent_institution_name,
    a_correspondent_country_code,
    a_correspondent_type,
    a_routing_ssi_source,
    a_beneficiary_account_number_in_correspondent_bic,
    a_is_preferred_correspondent,
    a_start_date,
    a_end_date
) ON CONFLICT
ON CONSTRAINT uq_routing_ssi_bic_currency_country_ssi_source
DO UPDATE SET
    beneficiary_institution_name = excluded.beneficiary_institution_name,
    beneficiary_institution_city = excluded.beneficiary_institution_city,
    beneficiary_institution_country_code = excluded.beneficiary_institution_country_code,
    asset_category = excluded.asset_category,
    correspondent_institution_name = excluded.correspondent_institution_name,
    correspondent_country_code = excluded.correspondent_country_code,
    correspondent_type = excluded.correspondent_type,
    beneficiary_account_number_in_correspondent_bic =
        excluded.beneficiary_account_number_in_correspondent_bic,
    is_preferred_correspondent = excluded.is_preferred_correspondent,
    start_date = excluded.start_date,
    end_date = excluded.end_date,
    update_ts = date_trunc('milliseconds', current_timestamp)
RETURNING routing_ssi_id;
$$;

CREATE OR REPLACE FUNCTION payment.is_valid_iban(a_iban varchar(34))
RETURNS bool
LANGUAGE sql AS $$
WITH translate (alphanum, number) AS (
VALUES
    ('A', 10), ('B', 11), ('C', 12), ('D', 13), ('E', 14), ('F', 15), ('G', 16),
    ('H', 17), ('I', 18), ('J', 19), ('K', 20), ('L', 21), ('M', 22), ('N', 23),
    ('O', 24), ('P', 25), ('Q', 26), ('R', 27), ('S', 28), ('T', 29), ('U', 30),
    ('V', 31), ('W', 32), ('X', 33), ('Y', 34), ('Z', 35),
    ('0', 0), ('1', 1), ('2', 2), ('3', 3), ('4', 4), ('5', 5), ('6', 6), ('7', 7),
    ('8', 8), ('9', 9)
)
-- Verify IBAN check digits
SELECT string_agg(tr.number::text, '')::numeric % 97 = 1
-- Rearrange IBAN (move IBAN country code and check digits to the end of IBAN)
FROM regexp_split_to_table(substr(a_iban, 5) || substr(a_iban, 1, 4), '') ch
    -- Translate IBAN letters into number
    JOIN translate tr ON tr.alphanum = ch;
$$;

CREATE OR REPLACE FUNCTION payment.validate_iban(a_iban varchar(34))
RETURNS TABLE (
    iban varchar(34),
    iban_country_code varchar(2),
    iban_check_digits varchar(2),
    iban_national_id varchar(30),
    bank_identifier varchar(30),
    branch_identifier varchar(30),
    account_number varchar(30),
    institution_name varchar(105),
    institution_country_name varchar(70),
    institution_country_code varchar(2),
    iban_bic varchar(11),
    is_sepa bool
)
LANGUAGE sql AS $$
    SELECT a_iban,
        ib.iban_country_code,
        ib.iban_check_digits,
        ib.iban_national_id,
        ib.bank_identifier,
        ib.branch_identifier,
        ib.account_number,
        iin.institution_name,
        iin.institution_country_name,
        iin.institution_country_code,
        iin.iban_bic,
        iin.is_sepa
    FROM payment.iban_structure ist,
        -- Parse IBAN structural components
        LATERAL (SELECT substr(a_iban, 1, 2) iban_country_code,
            substr(a_iban, 3, 2) iban_check_digits,
            substr(a_iban, ist.bank_identifier_position,
                ist.iban_national_id_length) iban_national_id,
            substr(a_iban, ist.bank_identifier_position,
                ist.bank_identifier_length) bank_identifier,
            substr(a_iban, ist.branch_identifier_position,
                ist.branch_identifier_length) branch_identifier,
            substr(a_iban, ist.account_number_position,
                ist.account_number_length) account_number
        ) ib
        -- Lookup IBAN BIC and instituiton details
        JOIN payment.iban_institution iin ON
            iin.iban_country_code = ib.iban_country_code
                AND iin.iban_national_id = ib.iban_national_id
    -- Lookup country code in IBAN structure
    WHERE ist.iban_country_code = ib.iban_country_code
        -- Validate IBAN length
        AND length(a_iban) = ist.iban_total_length
        -- Validate IBAN check digits
        AND payment.is_valid_iban(upper(a_iban))
        -- Check that the IBAN national ID is not in the IBAN exclusion list
        AND NOT EXISTS (SELECT 1
            FROM payment.iban_exclusion_list iex
            WHERE iex.iban_country_code = ib.iban_country_code
               AND iex.iban_national_id = ib.iban_national_id);
$$;

CREATE OR REPLACE FUNCTION payment.get_routing_ssi(
    a_beneficiary_bic varchar(11),
    a_currency_code varchar(3),
    a_country_code varchar(2)
) RETURNS TABLE (
    routing_ssi_id uuid,
    beneficiary_bic varchar(11),
    currency_code varchar(3),
    beneficiary_country_code varchar(2),
    correspondent_bic varchar(11),
    correspondent_country_code varchar(2),
    correspondent_type payment.correspondent_type_t,
    routing_ssi_source payment.routing_ssi_source_type,
    is_preferred_correspondent bool,
    holiday_date date,
    holiday_type payment.holiday_type_type,
    -- SEPA routing
    sepa_scheme varchar(8),
    payment_channel varchar(11),
    is_preferred_channel bool,
    beneficiary_institution_reachability
        payment.beneficiary_institution_reachability_type,
    intermediaryBic varchar(11)
) LANGUAGE sql AS $$
WITH RECURSIVE routing_ssi(
    routing_ssi_id,
    beneficiary_bic,
    currency_code,
    beneficiary_country_code,
    correspondent_bic,
    correspondent_country_code,
    correspondent_type,
    routing_ssi_source,
    is_preferred_correspondent,
    holiday_date,
    holiday_type,
    -- SEPA routing
    sepa_scheme,
    payment_channel,
    is_preferred_channel,
    beneficiary_institution_reachability,
    intermediaryBic
) AS (
    -- Routing SSI chain label for routing SSI chain aggregation
    SELECT ossi.routing_ssi_id,
        ossi.beneficiary_bic,
        ossi.currency_code,
        ossi.beneficiary_institution_country_code,
        ossi.correspondent_bic,
        ossi.correspondent_country_code,
        ossi.correspondent_type,
        ossi.routing_ssi_source,
        ossi.is_preferred_correspondent,
        obh.holiday_date,
        obh.holiday_type,
        -- SEPA routing
        osr.sepa_scheme,
        osr.payment_channel,
        osr.is_preferred_channel,
        osr.beneficiary_institution_reachability,
        osr.intermediaryBic
    FROM payment.routing_ssi ossi
        -- Check holidays for beneficiary institution
        LEFT JOIN payment.bank_holiday obh
            ON obh.country_code = ossi.beneficiary_institution_country_code
            AND obh.holiday_date <@ daterange(
                current_date, (current_date + INTERVAL '3 days')::date)
        -- Add SEPA routing scheme and payment channel if available
        -- for final CORRESPONDENT only
        LEFT JOIN payment.sepa_routing osr
            ON osr.beneficiary_bic = ossi.beneficiary_bic
            AND osr.beneficiary_institution_country_code =
                ossi.beneficiary_institution_country_code
            AND ossi.correspondent_type = 'CORRESPONDENT'
    -- Start with beneficiary BIC, term currency, and destination (beneficiary) country
    WHERE ossi.beneficiary_bic = a_beneficiary_bic
        AND ossi.currency_code = a_currency_code
        AND ossi.beneficiary_institution_country_code = a_country_code
    UNION
    SELECT ossi.routing_ssi_id,
        cssi.beneficiary_bic,
        cssi.currency_code,
        ossi.beneficiary_country_code,
        cssi.correspondent_bic,
        cssi.correspondent_country_code,
        cssi.correspondent_type,
        ossi.routing_ssi_source,
        cssi.is_preferred_correspondent,
        cbh.holiday_date,
        cbh.holiday_type,
        -- SEPA routing
        csr.sepa_scheme,
        csr.payment_channel,
        csr.is_preferred_channel,
        csr.beneficiary_institution_reachability,
        csr.intermediaryBic
    FROM routing_ssi ossi
        JOIN payment.routing_ssi cssi ON
            -- follow the routing SSI chain through LOCAL_CORRESPONDENTs
            ossi.correspondent_bic = cssi.beneficiary_bic
            AND ossi.correspondent_type = 'LOCAL_CORRESPONDENT'
        -- Check holidays for correspondent instituiton
        LEFT JOIN payment.bank_holiday cbh
            ON cbh.country_code = cssi.beneficiary_institution_country_code
            AND cbh.holiday_date <@ daterange(
                current_date, (current_date + INTERVAL '3 days')::date)
        -- Add SEPA routing scheme and payment channel if available
        -- for final CORRESPONDENT only
        LEFT JOIN payment.sepa_routing csr
            ON csr.beneficiary_bic = cssi.beneficiary_bic
            AND csr.beneficiary_institution_country_code =
                cssi.beneficiary_institution_country_code
            AND cssi.correspondent_type = 'CORRESPONDENT'
    WHERE cssi.currency_code = a_currency_code
)
SELECT ssi.routing_ssi_id,
    ssi.beneficiary_bic,
    ssi.currency_code,
    ssi.beneficiary_country_code,
    ssi.correspondent_bic,
    ssi.correspondent_country_code,
    ssi.correspondent_type,
    ssi.routing_ssi_source,
    ssi.is_preferred_correspondent,
    ssi.holiday_date,
    ssi.holiday_type,
    -- SEPA routing
    ssi.sepa_scheme,
    ssi.payment_channel,
    ssi.is_preferred_channel,
    ssi.beneficiary_institution_reachability,
    ssi.intermediaryBic
FROM routing_ssi ssi
-- Prioritize by SSI source, show evary SSI routing option one after another
ORDER BY ssi.routing_ssi_source, ssi.routing_ssi_id;
$$;
