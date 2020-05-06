SELECT pricing.put_pricing_rule(
    a_rule_name := 'UK country variable fee percentage',
    a_rule_key := 'UK',
    a_variable_fee := 0.01000,
    a_pricing_rule_id := '6a055c23-f369-4386-94ea-ffae7f1d1089'
    -- a_parent_rule_id := '5d3fae7d-fa22-43bd-a35c-55b2d0b1f8bd'
) pricing_rule_id;

SELECT * FROM pricing.pricing_rule;
