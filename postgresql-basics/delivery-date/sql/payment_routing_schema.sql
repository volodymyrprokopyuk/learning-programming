CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE SCHEMA payment_routing;

-- Data

CREATE TABLE payment_routing.pagofx_correspondent (
    pagofx_correspondent_id uuid NOT NULL
        DEFAULT gen_random_uuid(),
    correspondent_bic varchar(50) NOT NULL,
    country varchar(50) NOT NULL,
    settlement_days integer[] NOT NULL
        DEFAULT '{0, 1, 2, 3, 4, 5, 6}',
    -- Time in days taken by the PagoFX correspondent to process the payment
    value_date integer NOT NULL
        DEFAULT 0,
    -- Closing hours in a local time of the PagoFX correspondent used to
    -- calculate the delivery date estimation for the customer
    cutoff_time time NOT NULL
        DEFAULT '24:00:00',
    -- Time zone of the PagoFX correspondent used to calculate the cutoff_time
    time_zone varchar(50) NOT NULL,
    -- Correspondent priority to rank multiple applicable correspondnets.
    -- Lower value takes priority (ex: 4 is better than 10)
    creation_ts timestamptz NOT NULL
        DEFAULT current_timestamp(3),
    update_ts timestamptz NOT NULL
        DEFAULT current_timestamp(3),
    CONSTRAINT pk_pagofx_correspondent_id
        PRIMARY KEY (pagofx_correspondent_id),
    CONSTRAINT uq_pagofx_correspondent
        UNIQUE (correspondent_bic),
    CONSTRAINT ck_settlement_days_are_valid_week_days
        CHECK (settlement_days <@ '{0, 1, 2, 3, 4, 5, 6}')
);

CREATE TABLE payment_routing.bank_holiday (
    -- Country of the bank holiday
    country varchar(50) NOT NULL,
    -- Bank holiday date
    holiday_date date NOT NULL,
    -- Bank holiday time zone
    time_zone varchar(50) NOT NULL,
    creation_ts timestamptz NOT NULL
        DEFAULT current_timestamp(3),
    update_ts timestamptz NOT NULL
        DEFAULT current_timestamp(3),
    CONSTRAINT uq_country_holiday_date
        UNIQUE (country, holiday_date)
);

-- Data abstraction interface

CREATE OR REPLACE FUNCTION payment_routing.put_pagofx_correspondent (
    a_correspondent_bic varchar(50),
    a_country varchar(50),
    a_settlement_days integer[] DEFAULT '{0, 1, 2, 3, 4, 5, 6}',
    a_value_date integer DEFAULT 0,
    a_cutoff_time timetz DEFAULT '24:00:00',
    a_time_zone varchar(50) DEFAULT '00:00'
) RETURNS uuid
LANGUAGE sql AS $$
INSERT INTO payment_routing.pagofx_correspondent (
    correspondent_bic,
    country,
    settlement_days,
    value_date,
    cutoff_time,
    time_zone
) VALUES (
    a_correspondent_bic,
    a_country,
    a_settlement_days,
    a_value_date,
    a_cutoff_time,
    a_time_zone
) ON CONFLICT ON CONSTRAINT uq_pagofx_correspondent DO UPDATE SET
    country = excluded.country,
    settlement_days = excluded.settlement_days,
    value_date = excluded.value_date,
    cutoff_time = excluded.cutoff_time,
    time_zone = excluded.time_zone,
    update_ts = current_timestamp(3)
RETURNING pagofx_correspondent_id;
$$;

CREATE OR REPLACE FUNCTION payment_routing.put_bank_holiday (
    a_country varchar(50),
    a_holiday_date date,
    a_time_zone varchar(50)
) RETURNS void
LANGUAGE sql AS $$
INSERT INTO payment_routing.bank_holiday (country, holiday_date, time_zone)
VALUES (a_country, a_holiday_date, a_time_zone)
ON CONFLICT ON CONSTRAINT uq_country_holiday_date DO UPDATE SET
    time_zone = excluded.time_zone,
    update_ts = current_timestamp(3);
$$;

CREATE OR REPLACE FUNCTION payment_routing.get_estimated_delivery_date(
    a_correspondent_bics varchar(50)[],
    a_settlement_date timestamptz DEFAULT current_timestamp
) RETURNS TABLE (
    pagofx_correspondent_id uuid,
    correspondent_bic varchar(50),
    country varchar(50),
    settlement_days integer[],
    value_date integer,
    cutoff_time time,
    time_zone varchar(50)
    -- delivery_date timestamptz
) LANGUAGE sql AS $$
WITH correspondent (
    pagofx_correspondent_id,
    correspondent_bic,
    country,
    settlement_days,
    value_date,
    cutoff_time,
    time_zone,
    settlement_day
) AS (
SELECT c.pagofx_correspondent_id,
    c.correspondent_bic,
    c.country,
    c.settlement_days,
    c.value_date,
    c.cutoff_time,
    c.time_zone,
    d.date
FROM payment_routing.pagofx_correspondent c,
    (SELECT current_date + d.n FROM generate_series(0, 14, 1) d(n)) d(date)
WHERE c.correspondent_bic = ANY ('{BIC-ESBANK}')
    AND extract(dow FROM d.date) = ANY (c.settlement_days)
)

SELECT c.*, c.value_date delivery_date
FROM correspondent c
    LEFT JOIN payment_routing.bank_holiday bh
        ON bh.country = c.country AND bh.holiday_date = c.settlement_day
WHERE bh.holiday_date IS NULL
;
$$;


WITH available_settlement_day AS (
SELECT c.correspondent_bic,
    c.country,
    c.settlement_days,
    c.value_date,
    c.cutoff_time,
    c.time_zone,
    d.date settlement_day
FROM payment_routing.pagofx_correspondent c,
    (SELECT current_date + d.n FROM generate_series(0, 14, 1) d(n)) d(date)
WHERE c.correspondent_bic = ANY ('{BIC-ESBANK, BIC-USBANK}')
    AND extract(dow FROM d.date) = ANY (c.settlement_days)),

effective_value_date AS (
SELECT sd.*,
    (sd.value_date +
    CASE
        WHEN ('2020-11-01 24:00:00 Europe/Madrid'::timestamptz
            AT TIME ZONE 'America/New_York')::time > sd.cutoff_time THEN 1
        ELSE 0
    END
    ) effective_value_date,
    rank() OVER (PARTITION BY sd.correspondent_bic ORDER BY sd.settlement_day) rank
FROM available_settlement_day sd
    LEFT JOIN payment_routing.bank_holiday bh
        ON bh.country = sd.country AND bh.holiday_date = sd.settlement_day
WHERE bh.holiday_date IS NULL)

SELECT vd.*, vd.rank = vd.effective_value_date + 1
FROM effective_value_date vd
;
