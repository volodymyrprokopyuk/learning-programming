SELECT payment_routing.put_pagofx_correspondent (
    a_correspondent_bic := 'BIC-1',
    a_correspondent_legal_entity := 'LENT-CORR-1',
    a_pagofx_legal_entity := 'LENT-PFX-A',
    a_payment_channel := 'SWIFT',
    a_payment_message_template := 'MT-BIC-1-SWIFT',
    a_clearing_scheme := '{SCH-A1, SCH-A2}',
    a_value_currency := 'EUR',

    a_value_range := '[0,2000)',
    a_settlement_days := '{4, 5}',
    a_value_date := 1,
    a_cutoff_time := '17:00:00',
    a_correspondent_priority := 4
) pagofx_correspondent_id;

SELECT payment_routing.put_pagofx_correspondent (
    a_correspondent_bic := 'BIC-2',
    a_correspondent_legal_entity := 'LENT-CORR-2',
    a_pagofx_legal_entity := 'LENT-PFX-A',
    a_payment_channel := 'CURRENCY-CLOUD',
    a_payment_message_template := 'MT-BIC-2-CURRENCY-CLOUD',
    a_clearing_scheme := '{SCH-B1, SCH-B2}',
    a_value_currency := 'EUR',

    a_value_range := '[0,1000)',
    a_settlement_days := '{1, 2, 3, 4, 5}',
    a_value_date := 2,
    a_cutoff_time := '21:00:00',
    a_correspondent_priority := 8
) pagofx_correspondent_id;

SELECT pc.correspondent_bic bic,
    pc.correspondent_legal_entity corr_entity,
    pc.pagofx_legal_entity pfx_entity,
    pc.payment_channel channel,
    pc.payment_message_template "template",
    pc.clearing_scheme scheme,
    pc.value_currency currency,
    pc.value_range "range",
    pc.settlement_days days,
    pc.value_date vdate,
    pc.cutoff_time cutoff,
    pc.correspondent_priority priority
FROM payment_routing.get_pagofx_correspondent(
    a_value_currency := 'EUR',
    a_value_amount := 100,
    a_pagofx_legal_entity := 'LENT-PFX-A',
    a_clearing_schemes := '{SCH-A1, SCH-B2, SCH-AX}',
    a_correspondent_bics := '{BIC-1, BIC-2, BIC-X}',
    a_payment_channels := '{SWIFT, CURRENCY-CLOUD, CHANNEL-X}'
) pc;
