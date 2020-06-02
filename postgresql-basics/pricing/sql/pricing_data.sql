-- BASE
SELECT pricing.put_pricing_rule(
    'BASE', 'BASE', 0.10000,
    'f6d6e67b-16f6-4214-b135-9845f72516d0'
) pricing_rule_id;

-- UK
SELECT pricing.put_pricing_rule(
    'BASE > UK', 'UK', 0.01000,
    '6a055c23-f369-4386-94ea-ffae7f1d1089', 'f6d6e67b-16f6-4214-b135-9845f72516d0'
) pricing_rule_id;

SELECT pricing.put_pricing_rule(
    'BASE > UK > GBPEUR', 'GBPEUR', 0.01100,
    '5d3fae7d-fa22-43bd-a35c-55b2d0b1f8bd', '6a055c23-f369-4386-94ea-ffae7f1d1089'
) pricing_rule_id;

SELECT pricing.put_pricing_rule(
    'BASE > UK > GBPEUR > (,1000)', '(,1000)', 0.01300,
    '92cda7dc-3870-45aa-8ea8-dbf1ccb821d4', '5d3fae7d-fa22-43bd-a35c-55b2d0b1f8bd'
) pricing_rule_id;

SELECT pricing.put_pricing_rule(
    'BASE > UK > GBPEUR > (,1000) > CREDIT_CARD', 'CREDIT_CARD', 0.01600,
    'b12e00f1-aca1-4cb2-b78d-9825f19840aa', '92cda7dc-3870-45aa-8ea8-dbf1ccb821d4'
) pricing_rule_id;

SELECT pricing.put_pricing_rule(
    'BASE > UK > GBPEUR > (,1000) > BANK_ACCOUNT', 'BANK_ACCOUNT', 0.01700,
    'dee6ab99-dc5c-4cf5-8bcc-da818c346781', '92cda7dc-3870-45aa-8ea8-dbf1ccb821d4'
) pricing_rule_id;

SELECT pricing.put_pricing_rule(
    'BASE > UK > GBPEUR > [1000,5000)', '[1000,5000)', 0.01400,
    '58c3c007-0e01-45d1-b6ae-23d9a826a8de', '5d3fae7d-fa22-43bd-a35c-55b2d0b1f8bd'
) pricing_rule_id;

SELECT pricing.put_pricing_rule(
    'BASE > UK > GBPEUR > [5000,)', '[5000,)', 0.01500,
    '9671f6c9-3d3d-466e-82c5-41bf8e3ea852', '5d3fae7d-fa22-43bd-a35c-55b2d0b1f8bd'
) pricing_rule_id;

SELECT pricing.put_pricing_rule(
    'BASE > UK > EURGBP', 'EURGBP', 0.01200,
    '7bef4540-7607-4f32-977a-3affa3390de3', '6a055c23-f369-4386-94ea-ffae7f1d1089'
) pricing_rule_id;

-- BE
SELECT pricing.put_pricing_rule(
    'BASE > BE', 'BE', 0.02000,
    '5aae4e22-4e3a-4758-aa3e-240f2444b02a', 'f6d6e67b-16f6-4214-b135-9845f72516d0'
) pricing_rule_id;

SELECT pricing.put_pricing_rule(
    'BASE > BE > EURUSD', 'EURUSD', 0.02100,
    '0be9a9c8-41e6-4132-a836-496acdc33386', '5aae4e22-4e3a-4758-aa3e-240f2444b02a'
) pricing_rule_id;

SELECT pricing.put_pricing_rule(
    'BASE > BE > EURUSD > (,2000)', '(,2000)', 0.02300,
    '6c503647-04b5-4eea-ab7f-a0d1479ecb7e', '0be9a9c8-41e6-4132-a836-496acdc33386'
) pricing_rule_id;

SELECT pricing.put_pricing_rule(
    'BASE > BE > EURUSD > [2000,)', '[2000,)', 0.02400,
    '9137e251-7d67-4d79-991a-cebd2a8d1d38', '0be9a9c8-41e6-4132-a836-496acdc33386'
) pricing_rule_id;

SELECT pricing.put_pricing_rule(
    'BASE > BE > USDEUR', 'USDEUR', 0.02200,
    'c9947675-ee77-4d55-ae96-ffef7d9d0850', '5aae4e22-4e3a-4758-aa3e-240f2444b02a'
) pricing_rule_id;

SELECT pricing.put_pricing_rule(
    'BASE > BE > USDEUR > CREDIT_CARD', 'CREDIT_CARD', 0.02500,
    'ff30b3e6-5e34-47c0-ba57-179fe447761d', 'c9947675-ee77-4d55-ae96-ffef7d9d0850'
) pricing_rule_id;

SELECT pricing.put_pricing_rule(
    'BASE > BE > USDEUR > BANK_ACCOUNT', 'BANK_ACCOUNT', 0.02600,
    '6b429218-990a-457d-a4ae-87e614166e7b', 'c9947675-ee77-4d55-ae96-ffef7d9d0850'
) pricing_rule_id;

-- PL
SELECT pricing.put_pricing_rule(
    'BASE > PL', 'PL', 0.03000,
    'd76cefb9-f5d5-4d23-8447-62857c3b5d74', 'f6d6e67b-16f6-4214-b135-9845f72516d0'
) pricing_rule_id;

SELECT pricing.put_pricing_rule(
    'BASE > PL > PLNEUR', 'PLNEUR', 0.03100,
    '23530076-c52d-4d6e-94e7-ff606470ded2', 'd76cefb9-f5d5-4d23-8447-62857c3b5d74'
) pricing_rule_id;

-- ** PL

SELECT vfb.rule_name, vfb.rule_key, vfb.variable_fee, vfb.variable_fee_total
FROM pricing.get_variable_fee_breakdown('PL', 'PLNEUR', 100.00) vfb;

SELECT tfb.base_amount "base*", tfb.base_currency bcurrency,
    tfb.variable_fee_percentage fee, tfb.variable_fee_amount fee_amount,
    tfb.principal, tfb.rate,
    tfb.term_amount term, tfb.term_currency tcurrency
FROM pricing.get_term_from_base('PL', 'PLNEUR', 100.00, 1.0) tfb;

SELECT bab.residence_country, bab.currency_corridor, bab.funding_method,
    bab.base_amount_range
FROM pricing.get_base_amount_bands('PL', 'PLNEUR') bab;

SELECT tab.residence_country, tab.currency_corridor, tab.funding_method,
    tab.term_amount_range, tab.variable_fee_percentage fee
FROM pricing.get_term_amount_bands('PL', 'PLNEUR', 1.0) tab;

SELECT bft.base_amount base, bft.base_currency bcurrency,
    bft.variable_fee_percentage fee, bft.variable_fee_amount fee_amount,
    bft.principal, bft.rate,
    bft.term_amount "term*", bft.term_currency tcurrency
FROM pricing.get_base_from_term('PL', 'PLNEUR', 83.90, 1.0) bft;

-- ** BE

SELECT vfb.rule_name, vfb.rule_key, vfb.variable_fee, vfb.variable_fee_total
FROM pricing.get_variable_fee_breakdown('BE', 'EURUSD', 100.00) vfb;

SELECT tfb.base_amount "base*", tfb.base_currency bcurrency,
    tfb.variable_fee_percentage fee, tfb.variable_fee_amount fee_amount,
    tfb.principal, tfb.rate,
    tfb.term_amount term, tfb.term_currency tcurrency
FROM pricing.get_term_from_base('BE', 'EURUSD', 100.00, 1.0) tfb;

SELECT bab.residence_country, bab.currency_corridor, bab.funding_method,
    bab.base_amount_range
FROM pricing.get_base_amount_bands('BE', 'EURUSD') bab;

SELECT tab.residence_country, tab.currency_corridor, tab.funding_method,
    tab.term_amount_range, tab.variable_fee_percentage fee
FROM pricing.get_term_amount_bands('BE', 'EURUSD', 1.0) tab;

SELECT bft.base_amount base, bft.base_currency bcurrency,
    bft.variable_fee_percentage fee, bft.variable_fee_amount fee_amount,
    bft.principal, bft.rate,
    bft.term_amount "term*", bft.term_currency tcurrency
FROM pricing.get_base_from_term('BE', 'EURUSD', 83.60, 1.0) bft;

-- ** BE edge

SELECT vfb.rule_name, vfb.rule_key, vfb.variable_fee, vfb.variable_fee_total
FROM pricing.get_variable_fee_breakdown('BE', 'EURUSD', 1999.00) vfb;

SELECT tfb.base_amount "base*", tfb.base_currency bcurrency,
    tfb.variable_fee_percentage fee, tfb.variable_fee_amount fee_amount,
    tfb.principal, tfb.rate,
    tfb.term_amount term, tfb.term_currency tcurrency
FROM pricing.get_term_from_base('BE', 'EURUSD', 1999.00, 1.0) tfb;

SELECT bab.residence_country, bab.currency_corridor, bab.funding_method,
    bab.base_amount_range
FROM pricing.get_base_amount_bands('BE', 'EURUSD') bab;

SELECT tab.residence_country, tab.currency_corridor, tab.funding_method,
    tab.term_amount_range, tab.variable_fee_percentage fee
FROM pricing.get_term_amount_bands('BE', 'EURUSD', 1.0) tab;

SELECT bft.base_amount base, bft.base_currency bcurrency,
    bft.variable_fee_percentage fee, bft.variable_fee_amount fee_amount,
    bft.principal, bft.rate,
    bft.term_amount "term*", bft.term_currency tcurrency
FROM pricing.get_base_from_term('BE', 'EURUSD', 1671.16, 1.0) bft;

-- ** UK

SELECT vfb.rule_name, vfb.rule_key, vfb.variable_fee, vfb.variable_fee_total
FROM pricing.get_variable_fee_breakdown('UK', 'GBPEUR', 100.00, 'CREDIT_CARD') vfb;

SELECT tfb.base_amount "base*", tfb.base_currency bcurrency,
    tfb.variable_fee_percentage fee, tfb.variable_fee_amount fee_amount,
    tfb.principal, tfb.rate,
    tfb.term_amount term, tfb.term_currency tcurrency
FROM pricing.get_term_from_base('UK', 'GBPEUR', 100.00, 1.0, 'CREDIT_CARD') tfb;

SELECT bab.residence_country, bab.currency_corridor, bab.funding_method,
    bab.base_amount_range
FROM pricing.get_base_amount_bands('UK', 'GBPEUR', 'CREDIT_CARD') bab;

SELECT tab.residence_country, tab.currency_corridor, tab.funding_method,
    tab.term_amount_range, tab.variable_fee_percentage fee
FROM pricing.get_term_amount_bands('UK', 'GBPEUR', 1.0, 'CREDIT_CARD') tab;

SELECT bft.base_amount base, bft.base_currency bcurrency,
    bft.variable_fee_percentage fee, bft.variable_fee_amount fee_amount,
    bft.principal, bft.rate,
    bft.term_amount "term*", bft.term_currency tcurrency
FROM pricing.get_base_from_term('UK', 'GBPEUR', 85.00, 1.0, 'CREDIT_CARD') bft;

-- ** UK edge

SELECT vfb.rule_name, vfb.rule_key, vfb.variable_fee, vfb.variable_fee_total
FROM pricing.get_variable_fee_breakdown('UK', 'GBPEUR', 999.00, 'CREDIT_CARD') vfb;

SELECT tfb.base_amount "base*", tfb.base_currency bcurrency,
    tfb.variable_fee_percentage fee, tfb.variable_fee_amount fee_amount,
    tfb.principal, tfb.rate,
    tfb.term_amount term, tfb.term_currency tcurrency
FROM pricing.get_term_from_base('UK', 'GBPEUR', 999.00, 1.0, 'CREDIT_CARD') tfb;

SELECT bab.residence_country, bab.currency_corridor, bab.funding_method,
    bab.base_amount_range
FROM pricing.get_base_amount_bands('UK', 'GBPEUR', 'CREDIT_CARD') bab;

SELECT tab.residence_country, tab.currency_corridor, tab.funding_method,
    tab.term_amount_range, tab.variable_fee_percentage fee
FROM pricing.get_term_amount_bands('UK', 'GBPEUR', 1.0, 'CREDIT_CARD') tab;

SELECT bft.base_amount base, bft.base_currency bcurrency,
    bft.variable_fee_percentage fee, bft.variable_fee_amount fee_amount,
    bft.principal, bft.rate,
    bft.term_amount "term*", bft.term_currency tcurrency
FROM pricing.get_base_from_term('UK', 'GBPEUR', 849.15, 1.0, 'CREDIT_CARD') bft;
-- FROM pricing.get_base_from_term('UK', 'GBPEUR', 859.15, 1.0, 'CREDIT_CARD') bft;
