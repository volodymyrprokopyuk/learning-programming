SELECT payment_routing.put_pagofx_correspondent (
    a_correspondent_bic := 'BIC-A',
    a_correspondent_legal_entity := 'CORR-LENT-A',
    a_pagofx_legal_entity := 'PFX-LENT-A',
    a_payment_channel := 'SWIFT',
    a_payment_message_template := 'T-BIC-A-SWIFT',
    -- pagofx_correspondent_id uuid NOT NULL
    --     DEFAULT gen_random_uuid(),
    a_clearing_scheme := '{SCH-A1, SCH-A2}',
    a_value_currency := 'EUR'
    -- a_value_range numrange DEFAULT NULL,
        -- DEFAULT '[0,)',
    -- a_settlement_days integer[] DEFAULT NULL,
        -- DEFAULT '{0, 1, 2, 3, 4, 5, 6}',
    -- a_value_date integer DEFAULT NULL,
        -- DEFAULT 0,
    -- a_cutoff_time timetz DEFAULT NULL,
        -- DEFAULT '24:00:00',
    -- a_correspondent_priority integer DEFAULT NULL
) pagofx_correspondent_id;
