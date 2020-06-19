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

CREATE TABLE payment.swift_routing_ssi (
    swift_routing_ssi_id uuid NOT NULL
        DEFAULT gen_random_uuid(),
    owner_bic varchar(11) NOT NULL,
    owner_institution_name varchar(105) NOT NULL,
    owner_institution_city varchar(35) NOT NULL,
    owner_institution_country_code varchar(2) NOT NULL,
    currency_code varchar(3) NOT NULL,
    asset_category varchar(4) NOT NULL,
    -- BIC Account Holding Institution
    -- Final correspondent or intermediary correspondent
    -- BIC where the ownerBic holds the account
    -- lookup localCorrespondent as ownerBic to follow the SSI chain
    -- ownerBic has no direct correspondent relationship in the country of the currency,
    -- but works with a local correspondent as an intermediary
    correspondent_bic varchar(11) NOT NULL,
    correspondent_institution_name varchar(105) NOT NULL,
    correspondent_country_code varchar(2) NOT NULL,
    owner_account_number_in_correspondent_bic varchar(70),
    is_preferred_correspondent bool
        DEFAULT false,
    correspondent_type payment.correspondent_type_t NOT NULL,
    start_date date,
    end_date date,
    creation_ts timestamptz NOT NULL
        DEFAULT date_trunc('milliseconds', current_timestamp),
    update_ts timestamptz NOT NULL
        DEFAULT date_trunc('milliseconds', current_timestamp),
    CONSTRAINT pk_swift_routing_ssi
        PRIMARY KEY (swift_routing_ssi_id),
    CONSTRAINT uq_swift_routing_ssi_owner_bic_and_currency_code
        UNIQUE (owner_bic, currency_code)
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
    iban_bic = excluded.iban_bic,
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

CREATE OR REPLACE FUNCTION payment.put_swift_routing_ssi (
    a_owner_bic varchar(11),
    a_owner_institution_name varchar(105),
    a_owner_institution_city varchar(35),
    a_owner_institution_country_code varchar(2),
    a_currency_code varchar(3),
    a_asset_category varchar(4),
    a_correspondent_bic varchar(11),
    a_correspondent_institution_name varchar(105),
    a_correspondent_country_code varchar(2),
    a_correspondent_type payment.correspondent_type_t,
    a_owner_account_number_in_correspondent_bic varchar(70) DEFAULT NULL,
    a_is_preferred_correspondent bool DEFAULT false,
    a_start_date date DEFAULT NULL,
    a_end_date date DEFAULT NULL
) RETURNS uuid
LANGUAGE sql AS $$
INSERT INTO payment.swift_routing_ssi (
    owner_bic,
    owner_institution_name,
    owner_institution_city,
    owner_institution_country_code,
    currency_code,
    asset_category,
    correspondent_bic,
    correspondent_institution_name,
    correspondent_country_code,
    correspondent_type,
    owner_account_number_in_correspondent_bic,
    is_preferred_correspondent,
    start_date,
    end_date
 ) VALUES (
    a_owner_bic,
    a_owner_institution_name,
    a_owner_institution_city,
    a_owner_institution_country_code,
    a_currency_code,
    a_asset_category,
    a_correspondent_bic,
    a_correspondent_institution_name,
    a_correspondent_country_code,
    a_correspondent_type,
    a_owner_account_number_in_correspondent_bic,
    a_is_preferred_correspondent,
    a_start_date,
    a_end_date
) ON CONFLICT ON CONSTRAINT uq_swift_routing_ssi_owner_bic_and_currency_code
DO UPDATE SET
    owner_bic = excluded.owner_bic,
    owner_institution_name = excluded.owner_institution_name,
    owner_institution_city = excluded.owner_institution_city,
    owner_institution_country_code = excluded.owner_institution_country_code,
    currency_code = excluded.currency_code,
    asset_category = excluded.asset_category,
    correspondent_bic = excluded.correspondent_bic,
    correspondent_institution_name = excluded.correspondent_institution_name,
    correspondent_country_code = excluded.correspondent_country_code,
    correspondent_type = excluded.correspondent_type,
    owner_account_number_in_correspondent_bic =
        excluded.owner_account_number_in_correspondent_bic,
    is_preferred_correspondent = excluded.is_preferred_correspondent,
    start_date = excluded.start_date,
    end_date = excluded.end_date,
    update_ts = date_trunc('milliseconds', current_timestamp)
RETURNING swift_routing_ssi_id;
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
SELECT string_agg(tr.number::text, '')::numeric % 97 = 1
FROM regexp_split_to_table(substr(a_iban, 5) || substr(a_iban, 1, 4), '') ch
    JOIN translate tr ON tr.alphanum = ch;
$$;

CREATE OR REPLACE FUNCTION payment.validate_iban(a_iban varchar(34))
RETURNS bool
LANGUAGE sql AS $$
    SELECT length(a_iban) = ist.iban_total_length
        AND payment.is_valid_iban(upper(a_iban))
    FROM payment.iban_structure ist
    WHERE ist.iban_country_code = substr(a_iban, 1, 2)
$$;
