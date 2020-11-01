-- PagoFX correspondents

SELECT payment_routing.put_pagofx_correspondent (
    a_correspondent_bic := 'BIC-ESBANK',
    a_country := 'ES',
    a_settlement_days := '{1, 2, 3, 4, 5}',
    a_value_date := 1,
    a_cutoff_time := '17:00:00',
    -- a_time_zone := '01:00'
    a_time_zone := 'Europe/Madrid'
) pagofx_correspondent_id;

SELECT payment_routing.put_pagofx_correspondent (
    a_correspondent_bic := 'BIC-USBANK',
    a_country := 'US',
    a_settlement_days := '{1, 2, 3, 4, 5}',
    a_value_date := 2,
    a_cutoff_time := '19:00:00',
    -- a_time_zone := '-1 day, 19:00'
    a_time_zone := 'America/New_York'
) pagofx_correspondent_id;

-- Bank holidays

SELECT payment_routing.put_bank_holiday (
    a_country := 'ES',
    a_holiday_date := '2020-11-02',
    -- a_time_zone := '01:00'
    a_time_zone := 'Europe/Madrid'
);

SELECT payment_routing.put_bank_holiday (
    a_country := 'ES',
    a_holiday_date := '2020-11-10',
    -- a_time_zone := '01:00'
    a_time_zone := 'Europe/Madrid'
);

SELECT payment_routing.put_bank_holiday (
    a_country := 'US',
    a_holiday_date := '2020-11-01',
    -- a_time_zone := '-1 day, 19:00'
    a_time_zone := 'America/New_York'
);

-- Get estimated delivery date

-- SELECT dd.correspondent_bic,
--     dd.country,
--     dd.settlement_days,
--     dd.value_date,
--     dd.cutoff_time,
--     dd.time_zone,
--     dd.delivery_date
-- FROM payment_routing.get_estimated_delivery_date(
--     a_correspondent_bics := '{BIC-ESBANK, BIC-USBANK}'
-- ) dd;
