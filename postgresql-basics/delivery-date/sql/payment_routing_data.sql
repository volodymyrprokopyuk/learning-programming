-- PagoFX correspondents

SELECT payment_routing.put_pagofx_correspondent (
    a_correspondent_bic := 'BIC-PAGOFX',
    a_country := 'ES',
    a_settlement_days := '{1, 2, 3, 4, 5}',
    a_value_date := 0,
    a_cutoff_time := '22:00:00',
    a_time_zone := '01:00'
) pagofx_correspondent_id;

SELECT payment_routing.put_pagofx_correspondent (
    a_correspondent_bic := 'BIC-BOFA',
    a_country := 'US',
    a_settlement_days := '{1, 2, 3, 4, 5}',
    a_value_date := 1,
    a_cutoff_time := '18:00:00',
    a_time_zone := '-08:00'
) pagofx_correspondent_id;

-- Bank holidays

SELECT payment_routing.put_bank_holiday (
    a_country := 'ES',
    a_holiday := '2020-11-01 00:00:00',
    a_time_zone := '01:00'
);

SELECT payment_routing.put_bank_holiday (
    a_country := 'US',
    a_holiday := '2020-11-01 00:00:00',
    a_time_zone := '-08:00'
);

-- Get estimated delivery date

SELECT * FROM payment_routing.get_estimated_delivery_date(
    a_correspondent_bics := '{BIC-PAGOFX}'
)
