CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE SCHEMA payment_routing;

-- Data

CREATE TABLE payment_routing.pagofx_correspondent (
    pagofx_correspondent_id uuid NOT NULL
        DEFAULT gen_random_uuid(),
    correspondent_bic varchar(50) NOT NULL,
    -- PagoFX correspondent location country/area used to lookup bank holidays
    country varchar(50) NOT NULL,
    -- Days of week on which the PagoFX correspondent accepts payments
    -- 0 Sunday, 6 Saturday
    settlement_days integer[] NOT NULL
        DEFAULT '{0, 1, 2, 3, 4, 5, 6}',
    -- Time in days taken by the PagoFX correspondent to process the payment
    value_date integer NOT NULL
        DEFAULT 0,
    -- Closing hours in a local time of the PagoFX correspondent used to
    -- calculate the delivery date estimation for the customer
    cutoff_time time NOT NULL
        DEFAULT '24:00:00',
    -- Time zone of the PagoFX correspondent used to compare the cutoff_time
    time_zone varchar(50) NOT NULL,
    creation_ts timestamptz NOT NULL
        DEFAULT current_timestamp(3),
    update_ts timestamptz NOT NULL
        DEFAULT current_timestamp(3),
    CONSTRAINT pk_pagofx_correspondent_id
        PRIMARY KEY (pagofx_correspondent_id),
    CONSTRAINT uq_pagofx_correspondent
        UNIQUE (correspondent_bic),
    CONSTRAINT ck_settlement_days_are_valid_week_days
        CHECK (settlement_days <@ '{0, 1, 2, 3, 4, 5, 6}'),
    CONSTRAINT ck_value_date_is_non_negative
        CHECK (value_date >= 0)
);

CREATE TABLE payment_routing.bank_holiday (
    -- Country/area of the bank holiday
    country varchar(50) NOT NULL,
    -- Bank holiday date
    holiday_date date NOT NULL,
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
    a_holiday_date date
) RETURNS void
LANGUAGE sql AS $$
INSERT INTO payment_routing.bank_holiday (country, holiday_date)
VALUES (a_country, a_holiday_date)
ON CONFLICT ON CONSTRAINT uq_country_holiday_date DO UPDATE SET
    update_ts = current_timestamp(3);
$$;

CREATE OR REPLACE FUNCTION payment_routing.get_estimated_delivery_date(
    a_correspondent_bics varchar(50)[],
    a_settlement_date date DEFAULT current_date
) RETURNS TABLE (
    correspondent_bic varchar(50),
    country varchar(50),
    settlement_days integer[],
    value_date integer,
    cutoff_time time,
    time_zone varchar(50),
    delivery_date date
) LANGUAGE sql AS $$
-- Calculate the availalbe settlement days for each of the PagoFX correspondents
WITH available_settlement_day AS (
SELECT c.*, d.date settlement_day
FROM payment_routing.pagofx_correspondent c,
    -- All successive dates from the requested settlement date
    (SELECT a_settlement_date + d.n FROM generate_series(0, 14, 1) d(n)) d(date)
-- Select only requested PagoFX correspondents
WHERE c.correspondent_bic = ANY (a_correspondent_bics)
    -- Select only only days when the PagoFX correspondent is operating
    AND extract(dow FROM d.date) = ANY (c.settlement_days)),
-- Calculate the estimated value date for each of the PagoFX correspondents
-- taking into consideration bank holidays, time zone, and cutoff time
effective_value_date AS (
SELECT sd.*,
    -- Always add value date of the PagoFX correspondent to the effective value date
    (sd.value_date +
    -- Add 1 more day to the effective value date only if the settlement date is today
    -- and the settlement time is after the cutoff time in a time zone of the PagoFX
    -- correspondent
    CASE WHEN (a_settlement_date = current_date)
        AND (current_timestamp AT TIME ZONE sd.time_zone)::time > sd.cutoff_time
        THEN 1
        ELSE 0
    END) effective_value_date,
    -- Assign sequential numbers to the settlement days
    rank() OVER (PARTITION BY sd.correspondent_bic ORDER BY sd.settlement_day)
    settlement_day_rank
FROM available_settlement_day sd
    -- Attach bank holiday dates for a country/area of the PagoFX correspondent
    LEFT JOIN payment_routing.bank_holiday bh
        ON bh.country = sd.country AND bh.holiday_date = sd.settlement_day
-- Select only working days of the PagoFX correspondent
WHERE bh.holiday_date IS NULL)

SELECT vd.correspondent_bic,
    vd.country,
    vd.settlement_days,
    vd.value_date,
    vd.cutoff_time,
    vd.time_zone,
    vd.settlement_day delivery_date
FROM effective_value_date vd
-- Select the first available settlement day that satisfies the effective value date
WHERE vd.settlement_day_rank = vd.effective_value_date + 1;
$$;
