-- DEFAULT
SELECT pricing.put_pricing_rule(
    a_rule_name := 'DEFAULT',
    a_rule_key := 'DEFAULT',
    a_variable_fee := 0.10000,
    a_pricing_rule_id := 'f6d6e67b-16f6-4214-b135-9845f72516d0'
) pricing_rule_id;

-- UK
SELECT pricing.put_pricing_rule(
    a_rule_name := 'UK',
    a_rule_key := 'UK',
    a_variable_fee := 0.01000,
    a_pricing_rule_id := '6a055c23-f369-4386-94ea-ffae7f1d1089'
) pricing_rule_id;

SELECT pricing.put_pricing_rule(
    a_rule_name := 'UK, GBPEUR',
    a_rule_key := 'GBPEUR',
    a_variable_fee := 0.01100,
    a_pricing_rule_id := '5d3fae7d-fa22-43bd-a35c-55b2d0b1f8bd',
    a_parent_rule_id := '6a055c23-f369-4386-94ea-ffae7f1d1089'
) pricing_rule_id;

SELECT pricing.put_pricing_rule(
    a_rule_name := 'UK, EURGBP',
    a_rule_key := 'EURGBP',
    a_variable_fee := 0.01200,
    a_pricing_rule_id := '7bef4540-7607-4f32-977a-3affa3390de3',
    a_parent_rule_id := '6a055c23-f369-4386-94ea-ffae7f1d1089'
) pricing_rule_id;

SELECT pricing.put_pricing_rule(
    a_rule_name := 'UK, EURGBP, CREDIT_CARD',
    a_rule_key := 'CREDIT_CARD',
    a_variable_fee := 0.01300,
    a_pricing_rule_id := '000760b9-1bc4-4b79-be7d-c6cc3e53df38',
    a_parent_rule_id := '5d3fae7d-fa22-43bd-a35c-55b2d0b1f8bd'
) pricing_rule_id;

SELECT pricing.put_pricing_rule(
    a_rule_name := 'UK, EURGBP, BANK_ACCOUNT',
    a_rule_key := 'BANK_ACCOUNT',
    a_variable_fee := 0.01400,
    a_pricing_rule_id := 'b676f4de-0349-4918-9e08-6dca9eef45e0',
    a_parent_rule_id := '5d3fae7d-fa22-43bd-a35c-55b2d0b1f8bd'
) pricing_rule_id;

-- BE
SELECT pricing.put_pricing_rule(
    a_rule_name := 'BE',
    a_rule_key := 'BE',
    a_variable_fee := 0.02000,
    a_pricing_rule_id := '5aae4e22-4e3a-4758-aa3e-240f2444b02a'
) pricing_rule_id;

SELECT pricing.put_pricing_rule(
    a_rule_name := 'BE, EURUSD',
    a_rule_key := 'EURUSD',
    a_variable_fee := 0.02100,
    a_pricing_rule_id := '0be9a9c8-41e6-4132-a836-496acdc33386',
    a_parent_rule_id := '5aae4e22-4e3a-4758-aa3e-240f2444b02a'
) pricing_rule_id;

SELECT pricing.put_pricing_rule(
    a_rule_name := 'BE, USDEUR',
    a_rule_key := 'USDEUR',
    a_variable_fee := 0.02200,
    a_pricing_rule_id := 'c9947675-ee77-4d55-ae96-ffef7d9d0850',
    a_parent_rule_id := '5aae4e22-4e3a-4758-aa3e-240f2444b02a'
) pricing_rule_id;


SELECT vf.rule_name, vf.rule_key, vf.variable_fee, vf.parent_rule_id
FROM pricing.get_variable_fee(
    a_residence_country := 'UK',
    a_currency_corridor := 'GBPEUR',
    a_base_amount := 100,
    a_funding_method := 'CREDIT_CARD'
) vf;
