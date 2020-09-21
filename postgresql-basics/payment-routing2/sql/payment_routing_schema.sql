CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE SCHEMA payment_routing;

-- Data

CREATE TABLE payment_routing.pagofx_correspondent (
    pagofx_correspondent_id uuid NOT NULL
        DEFAULT gen_random_uuid(),
    correspondent_bic varchar(50) NOT NULL,
    -- Correspondnet legal entity for attributing entries in the ledger
    correspondent_legal_entity varchar(50) NOT NULL,
    -- PagoFX legal entity for attributing entries in the ledger
    pagofx_legal_entity varchar(50) NOT NULL,
    -- Technological channel to instruct payment (ex: CurrenyCloud, SWIFT)
    payment_channel varchar(50) NOT NULL,
    -- Relative path or logical symbol that identifies the payment message
    -- template to be used by the payment execution system
    payment_message_template varchar(200) NOT NULL,
    -- Clearing scheme in which the PagoFX correspondnet participates for
    -- the value currency withing the value range on settlement days.
    -- Clearing scheme may have multiple equivalent aliases
    clearing_scheme varchar(50)[] NOT NULL,
    -- Value currency for the clearing scheme
    value_currency varchar(3) NOT NULL,
    -- Value range supported by the PagoFX correspondnet. Any or both
    -- lower bound and upper bound can be specified
    value_range numrange NOT NULL
        DEFAULT '[0,)',
    -- Settlement days on which the clearing scheme is open (0 is Sunday)
    settlement_days integer[] NOT NULL
        DEFAULT '{0, 1, 2, 3, 4, 5, 6}',
    -- Time in days taken by the PagoFX correspondent to process the payment
    value_date integer NOT NULL
        DEFAULT 0,
    -- Closing hours in a time zone of the PagoFX correspondent used to
    -- refine the delivery date estimation for the customer
    cutoff_time timetz NOT NULL
        DEFAULT '24:00:00',
    -- Correspondent priority to rank multiple applicable correspondnets.
    -- Lower value takes priority (ex: 4 is better than 10)
    correspondent_priority integer NOT NULL
        DEFAULT 10,
    creation_ts timestamptz NOT NULL
        DEFAULT date_trunc('milliseconds', current_timestamp),
    update_ts timestamptz NOT NULL
        DEFAULT date_trunc('milliseconds', current_timestamp),
    CONSTRAINT pk_pagofx_correspondent_id
        PRIMARY KEY (pagofx_correspondent_id),
    CONSTRAINT uq_pagofx_correspondent
        UNIQUE (
            correspondent_bic,
            correspondent_legal_entity,
            pagofx_legal_entity,
            clearing_scheme,
            value_currency
        ),
    CONSTRAINT ck_settlement_days_are_valid_week_days
        CHECK (settlement_days <@ '{0, 1, 2, 3, 4, 5, 6}'),
    CONSTRAINT ck_value_range_is_positive
        CHECK (value_range <@ '[0,)')
);

-- Data abstraction interface

CREATE OR REPLACE FUNCTION payment_routing.put_pagofx_correspondent (
    a_correspondent_bic varchar(50),
    a_correspondent_legal_entity varchar(50),
    a_pagofx_legal_entity varchar(50),
    a_payment_channel varchar(50),
    a_payment_message_template varchar(200),
    a_clearing_scheme varchar(50)[],
    a_value_currency varchar(3),
    a_value_range numrange DEFAULT '[0,)',
    a_settlement_days integer[] DEFAULT '{0, 1, 2, 3, 4, 5, 6}',
    a_value_date integer DEFAULT 0,
    a_cutoff_time timetz DEFAULT '24:00:00',
    a_correspondent_priority integer DEFAULT 10
) RETURNS uuid
LANGUAGE sql AS $$
INSERT INTO payment_routing.pagofx_correspondent (
    correspondent_bic,
    correspondent_legal_entity,
    pagofx_legal_entity,
    payment_channel,
    payment_message_template,
    clearing_scheme,
    value_currency,
    value_range,
    settlement_days,
    value_date,
    cutoff_time,
    correspondent_priority
) VALUES (
    a_correspondent_bic,
    a_correspondent_legal_entity,
    a_pagofx_legal_entity,
    a_payment_channel,
    a_payment_message_template,
    a_clearing_scheme,
    a_value_currency,
    a_value_range,
    a_settlement_days,
    a_value_date,
    a_cutoff_time,
    a_correspondent_priority
) ON CONFLICT ON CONSTRAINT uq_pagofx_correspondent DO UPDATE SET
    payment_channel = excluded.payment_channel,
    payment_message_template = excluded.payment_message_template,
    value_range = excluded.value_range,
    settlement_days = excluded.settlement_days,
    value_date = excluded.value_date,
    cutoff_time = excluded.cutoff_time,
    correspondent_priority = excluded.correspondent_priority,
    update_ts = date_trunc('milliseconds', current_timestamp)
RETURNING pagofx_correspondent_id;
$$;

CREATE OR REPLACE FUNCTION payment_routing.get_pagofx_correspondent(
    a_value_currency varchar(3),
    a_value_amount numeric(15, 3),
    a_pagofx_legal_entity varchar(50),
    a_clearing_schemes varchar(50)[] DEFAULT NULL,
    a_correspondent_bics varchar(50)[] DEFAULT NULL,
    a_payment_channels varchar(50)[] DEFAULT NULL
) RETURNS TABLE (
    pagofx_correspondent_id uuid,
    correspondent_bic varchar(50),
    correspondent_legal_entity varchar(50),
    pagofx_legal_entity varchar(50),
    payment_channel varchar(50),
    payment_message_template varchar(200),
    clearing_scheme varchar(50)[],
    value_currency varchar(3),
    value_range numrange,
    settlement_days integer[],
    value_date integer,
    cutoff_time timetz,
    correspondent_priority integer
) LANGUAGE sql AS $$
SELECT pc.pagofx_correspondent_id,
    pc.correspondent_bic,
    pc.correspondent_legal_entity,
    pc.pagofx_legal_entity,
    pc.payment_channel,
    pc.payment_message_template,
    pc.clearing_scheme,
    pc.value_currency,
    pc.value_range,
    pc.settlement_days,
    pc.value_date,
    pc.cutoff_time,
    pc.correspondent_priority
FROM payment_routing.pagofx_correspondent pc
WHERE pc.value_currency = a_value_currency
    AND pc.value_range @> a_value_amount
    AND pc.pagofx_legal_entity = a_pagofx_legal_entity
    AND (a_clearing_schemes IS NULL
        OR pc.clearing_scheme && a_clearing_schemes)
    AND (a_correspondent_bics IS NULL
        OR pc.correspondent_bic = ANY (a_correspondent_bics))
    AND (a_payment_channels IS NULL
        OR pc.payment_channel = ANY (a_payment_channels))
ORDER BY pc.correspondent_priority;
$$;
