CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE SCHEMA pricing;

-- Data

CREATE TABLE pricing.pricing_rule (
    pricing_rule_id uuid NOT NULL
        DEFAULT gen_random_uuid(),
    rule_name varchar(100) NOT NULL,
    rule_key varchar(50) NOT NULL,
    variable_fee numeric(7, 5) NOT NULL
        DEFAULT 0.00000,
    creation_ts timestamptz NOT NULL
        DEFAULT date_trunc('milliseconds', current_timestamp),
    update_ts timestamptz NOT NULL
        DEFAULT date_trunc('milliseconds', current_timestamp),
    parent_rule_id uuid NULL,
    CONSTRAINT pk_pricing_rule
        PRIMARY KEY (pricing_rule_id),
    CONSTRAINT fk_pricing_rule_may_have_parent_pricing_rule
        FOREIGN KEY (parent_rule_id) REFERENCES pricing.pricing_rule (pricing_rule_id)
        ON UPDATE RESTRICT ON DELETE RESTRICT
);

-- Logic

-- SELECT pricing.put_pricing_rule(
--     a_rule_name := 'UK country variable fee percentage',
--     a_rule_key := 'UK',
--     a_variable_fee := 0.01000,
--     a_pricing_rule_id := '6a055c23-f369-4386-94ea-ffae7f1d1089',
--     a_parent_rule_id := '5d3fae7d-fa22-43bd-a35c-55b2d0b1f8bd'
-- ) pricing_rule_id;

CREATE OR REPLACE FUNCTION pricing.put_pricing_rule(
    a_rule_name varchar(100),
    a_rule_key varchar(50),
    a_variable_fee numeric(7, 5),
    a_pricing_rule_id uuid DEFAULT NULL,
    a_parent_rule_id uuid DEFAULT NULL
) RETURNS uuid
LANGUAGE sql AS $$
INSERT INTO pricing.pricing_rule (
    pricing_rule_id,
    rule_name,
    rule_key,
    variable_fee,
    parent_rule_id
) VALUES (
    coalesce(a_pricing_rule_id, gen_random_uuid()),
    a_rule_name,
    a_rule_key,
    a_variable_fee,
    a_parent_rule_id
) ON CONFLICT ON CONSTRAINT pk_pricing_rule DO UPDATE SET
    rule_name = excluded.rule_name,
    rule_key = excluded.rule_key,
    variable_fee = excluded.variable_fee,
    update_ts = date_trunc('milliseconds', current_timestamp),
    parent_rule_id = excluded.parent_rule_id
RETURNING pricing_rule_id;
$$;

-- SELECT vfb.*
-- FROM pricing.get_variable_fee_breakdown(
--     a_residence_country := 'UK',
--     a_currency_corridor := 'GBPEUR',
--     a_base_amount := 100,
--     a_funding_method := 'CREDIT_CARD'
-- ) vfb;

CREATE OR REPLACE FUNCTION pricing.get_variable_fee_breakdown(
    a_residence_country varchar(50),
    a_currency_corridor varchar(50),
    a_base_amount numeric(10, 2),
    a_funding_method varchar(50) DEFAULT 'UNKNOWN'
) RETURNS TABLE (
    pricing_rule_id uuid,
    rule_name varchar(100),
    rule_key varchar(50),
    variable_fee numeric(7, 5),
    variable_fee_total numeric(7, 5),
    creation_ts timestamptz,
    update_ts timestamptz,
    parent_rule_id uuid
) LANGUAGE sql AS $$
WITH RECURSIVE pricing_rule_chain(
    pricing_rule_id,
    rule_name,
    rule_key,
    variable_fee,
    creation_ts,
    update_ts,
    parent_rule_id
) AS (
    -- Pricing rule root
    SELECT pr.pricing_rule_id,
        pr.rule_name,
        pr.rule_key,
        pr.variable_fee,
        pr.creation_ts,
        pr.update_ts,
        pr.parent_rule_id
    FROM pricing.pricing_rule pr
    WHERE pr.rule_key = 'BASE'
    UNION
    -- Next pricing rule child
    SELECT npr.pricing_rule_id,
        npr.rule_name,
        npr.rule_key,
        npr.variable_fee,
        npr.creation_ts,
        npr.update_ts,
        npr.parent_rule_id
    FROM pricing.pricing_rule npr
        JOIN pricing_rule_chain prc ON prc.pricing_rule_id = npr.parent_rule_id
    WHERE npr.rule_key IN (a_residence_country, a_currency_corridor, a_funding_method)
        OR (npr.rule_key ~ '[\(\[]\d*, *\d*[\)\]]'
            AND a_base_amount <@ npr.rule_key::numrange)
)
SELECT prc.pricing_rule_id,
    prc.rule_name,
    prc.rule_key,
    prc.variable_fee,
    sum(prc.variable_fee) OVER (ORDER BY prc.rule_name) variable_fee_total,
    prc.creation_ts,
    prc.update_ts,
    prc.parent_rule_id
FROM pricing_rule_chain prc;
$$;

CREATE OR REPLACE FUNCTION pricing.get_term_from_base(
    a_residence_country varchar(50),
    a_currency_corridor varchar(50),
    a_base_amount numeric(10, 2),
    a_rate numeric(10, 5),
    a_funding_method varchar(50) DEFAULT 'UNKNOWN'
) RETURNS TABLE (
    base_amount numeric(10, 2),
    base_currency varchar(50),
    variable_fee_percentage numeric(7, 5),
    variable_fee_amount numeric(10, 2),
    principal numeric(10, 2),
    rate numeric(10, 5),
    term_amount numeric(10, 2),
    term_currency varchar(50)
) LANGUAGE sql AS $$
WITH variable_fee (
    variable_fee_total
) AS (
    SELECT vfb.variable_fee_total
    FROM pricing.get_variable_fee_breakdown(
        a_residence_country, a_currency_corridor, a_base_amount, a_funding_method
    ) vfb
    ORDER BY vfb.rule_name DESC
    LIMIT 1
)
SELECT a_base_amount,
    substring(a_currency_corridor from 1 for 3),
    vf.variable_fee_total,
    round(a_base_amount * vf.variable_fee_total, 2),
    round(a_base_amount - a_base_amount * vf.variable_fee_total, 2),
    a_rate,
    round((a_base_amount - a_base_amount * vf.variable_fee_total) * a_rate, 2),
    substring(a_currency_corridor from 4 for 3)
FROM variable_fee vf;
$$;

CREATE OR REPLACE FUNCTION pricing.get_base_amount_bands(
    a_residence_country varchar(50),
    a_currency_corridor varchar(50),
    a_funding_method varchar(50) DEFAULT 'UNKNOWN'
) RETURNS TABLE (
    residence_country varchar(50),
    currency_corridor varchar(50),
    funding_method varchar(50),
    base_amount_range numrange
) LANGUAGE sql AS $$
WITH RECURSIVE pricing_rule_chain(
    pricing_rule_id,
    rule_key,
    parent_rule_id
) AS (
    -- Pricing rule root
    SELECT pr.pricing_rule_id, pr.rule_key, pr.parent_rule_id
    FROM pricing.pricing_rule pr
    WHERE pr.rule_key = 'BASE'
    UNION
    -- Next pricing rule child
    SELECT npr.pricing_rule_id, npr.rule_key, npr.parent_rule_id
    FROM pricing.pricing_rule npr
        JOIN pricing_rule_chain prc ON prc.pricing_rule_id = npr.parent_rule_id
    WHERE npr.rule_key IN (a_residence_country, a_currency_corridor, a_funding_method)
        OR (npr.rule_key ~ '[\(\[]\d*, *\d*[\)\]]')
)
SELECT a_residence_country, a_currency_corridor, a_funding_method,
    prc.rule_key::numrange
FROM pricing_rule_chain prc
WHERE prc.rule_key ~ '[\(\[]\d*, *\d*[\)\]]'
UNION
-- Default unbounded base amount range
SELECT a_residence_country, a_currency_corridor, a_funding_method,
    '(,)'::numrange
WHERE NOT EXISTS (
    SELECT 1 FROM pricing_rule_chain WHERE rule_key ~ '[\(\[]\d*, *\d*[\)\]]'
);
$$;

CREATE OR REPLACE FUNCTION pricing.get_term_amount_bands(
    a_residence_country varchar(50),
    a_currency_corridor varchar(50),
    a_rate numeric(10, 5),
    a_funding_method varchar(50) DEFAULT 'UNKNOWN'
) RETURNS TABLE (
    residence_country varchar(50),
    currency_corridor varchar(50),
    funding_method varchar(50),
    term_amount_range numrange,
    variable_fee_percentage numeric(7, 5)
) LANGUAGE sql AS $$
SELECT bab.residence_country, bab.currency_corridor, bab.funding_method,
    -- Term amount range
    CASE
        -- Empty range
        WHEN isempty(bab.base_amount_range)
        THEN bab.base_amount_range
        -- Left unbounded range
        WHEN lower_inf(bab.base_amount_range)
        THEN numrange(
            NULL,
            (SELECT term_amount
                FROM pricing.get_term_from_base(
                a_residence_country, a_currency_corridor,
                upper(bab.base_amount_range) - 0.01, a_rate, a_funding_method)),
            '(]')
        -- Right unbounded range
        WHEN upper_inf(bab.base_amount_range)
        THEN numrange(
            (SELECT term_amount
                FROM pricing.get_term_from_base(
                a_residence_country, a_currency_corridor,
                lower(bab.base_amount_range), a_rate, a_funding_method)),
            NULL,
            '[)')
        -- Fully specified range
        ELSE numrange(
            (SELECT term_amount
                FROM pricing.get_term_from_base(
                a_residence_country, a_currency_corridor,
                lower(bab.base_amount_range), a_rate, a_funding_method)),
            (SELECT term_amount
                FROM pricing.get_term_from_base(
                a_residence_country, a_currency_corridor,
                upper(bab.base_amount_range) - 0.01, a_rate, a_funding_method)),
            '[]')
    END,
    -- Variable fee
    CASE
        -- Empty range
        WHEN isempty(bab.base_amount_range)
        THEN (SELECT variable_fee_percentage
                FROM pricing.get_term_from_base(
                a_residence_country, a_currency_corridor,
                1.0, a_rate, a_funding_method))
        -- Left unbounded range
        WHEN lower_inf(bab.base_amount_range)
        THEN (SELECT variable_fee_percentage
                FROM pricing.get_term_from_base(
                a_residence_country, a_currency_corridor,
                upper(bab.base_amount_range) - 0.01, a_rate, a_funding_method))
        -- Right unbounded range
        WHEN upper_inf(bab.base_amount_range)
        THEN (SELECT variable_fee_percentage
                FROM pricing.get_term_from_base(
                a_residence_country, a_currency_corridor,
                lower(bab.base_amount_range), a_rate, a_funding_method))
        -- Fully specified range
        ELSE (SELECT variable_fee_percentage
                FROM pricing.get_term_from_base(
                a_residence_country, a_currency_corridor,
                lower(bab.base_amount_range), a_rate, a_funding_method))
    END
FROM pricing.get_base_amount_bands(
    a_residence_country, a_currency_corridor, a_funding_method
) bab;
$$;

CREATE OR REPLACE FUNCTION pricing.get_base_from_term(
    a_residence_country varchar(50),
    a_currency_corridor varchar(50),
    a_term_amount numeric(10, 2),
    a_rate numeric(10, 5),
    a_funding_method varchar(50) DEFAULT 'UNKNOWN'
) RETURNS TABLE (
    base_amount numeric(10, 2),
    base_currency varchar(50),
    variable_fee_percentage numeric(7, 5),
    variable_fee_amount numeric(10, 2),
    principal numeric(10, 2),
    rate numeric(10, 5),
    term_amount numeric(10, 2),
    term_currency varchar(50)
) LANGUAGE sql AS $$
SELECT
    -- Base amount
    round((a_term_amount / a_rate) / (1 - tab.variable_fee_percentage), 2) base_amount,
    substring(a_currency_corridor from 1 for 3),
    tab.variable_fee_percentage,
    -- Variable fee amount
    round(((a_term_amount / a_rate) / (1 - tab.variable_fee_percentage))
        - (a_term_amount / a_rate), 2),
    -- Principal
    round(a_term_amount / a_rate, 2),
    a_rate,
    -- Term amount
    a_term_amount,
    substring(a_currency_corridor from 4 for 3)
FROM pricing.get_term_amount_bands(
   a_residence_country, a_currency_corridor, a_rate, a_funding_method) tab
WHERE a_term_amount <@ tab.term_amount_range
ORDER BY base_amount;
-- LIMIT 1;
$$;
