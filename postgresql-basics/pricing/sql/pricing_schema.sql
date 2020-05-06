CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE SCHEMA pricing;

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
        ON UPDATE RESTRICT ON DELETE RESTRICT,
    CONSTRAINT uq_no_pricing_rule_multiple_inheritance_and_no_cycles
        UNIQUE (pricing_rule_id),
    CONSTRAINT ch_no_pricing_rule_self_referencing
        CHECK (pricing_rule_id <> parent_rule_id)
);

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
)
RETURNS uuid
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
