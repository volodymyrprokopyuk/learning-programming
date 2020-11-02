-- PagoFX correspondents

SELECT payment_routing.put_pagofx_correspondent (
    a_correspondent_bic := 'BIC-ESBANK',
    a_country := 'ES',
    a_settlement_days := '{1, 2, 3, 4, 5}',
    a_value_date := 0,
    a_cutoff_time := '17:00:00',
    a_time_zone := 'Europe/Madrid'
    -- a_time_zone := '+01:00'
) pagofx_correspondent_id;

SELECT payment_routing.put_pagofx_correspondent (
    a_correspondent_bic := 'BIC-USBANK',
    a_country := 'US',
    a_settlement_days := '{1, 2, 3, 4, 5}',
    a_value_date := 0,
    a_cutoff_time := '19:00:00',
    a_time_zone := 'America/New_York'
    -- a_time_zone := '-06:00'
) pagofx_correspondent_id;

-- Bank holidays

SELECT payment_routing.put_bank_holiday ('ES', '2020-11-02');
SELECT payment_routing.put_bank_holiday ('US', '2020-11-06');

-- Get estimated delivery date

SELECT *
FROM payment_routing.get_estimated_delivery_date('{BIC-ESBANK, BIC-USBANK}');
-- FROM payment_routing.get_estimated_delivery_date('{BIC-ESBANK, BIC-USBANK}', '2020-11-06');
