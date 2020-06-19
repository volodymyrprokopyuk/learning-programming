-- IBAN structure

SELECT payment.put_iban_structure(
    a_iban_country_code := 'AD',
    a_iban_country_code_position := 1,
    a_iban_country_code_length := 2,
    a_iban_check_digits_position := 3,
    a_iban_check_digits_length := 2,
    a_bank_identifier_position := 5,
    a_bank_identifier_length := 4,
    a_branch_identifier_position := 9,
    a_branch_identifier_length := 4,
    a_iban_national_id_length := 8,
    a_account_number_position := 13,
    a_account_number_length := 12,
    a_iban_total_length := 24
) iban_structure_id;

SELECT payment.put_iban_structure(
    a_iban_country_code := 'BE',
    a_iban_country_code_position := 1,
    a_iban_country_code_length := 2,
    a_iban_check_digits_position := 3,
    a_iban_check_digits_length := 2,
    a_bank_identifier_position := 5,
    a_bank_identifier_length := 3,
    a_iban_national_id_length := 3,
    a_account_number_position := 5,
    a_account_number_length := 12,
    a_iban_total_length := 16
) iban_structure_id;

-- IBAN instituiton

SELECT payment.put_iban_institution(
    a_institution_name := 'MoraBanc',
    a_institution_country_name := 'ANDORRA',
    a_institution_country_code := 'AD',
    a_iban_bic := 'BINAADADXXX',
    a_routing_bic := 'BINAADADXXX',
    a_iban_national_id := '00070014',
    a_iban_country_code := 'AD'
) iban_institution_id;

-- IBAN exclusion list

SELECT payment.put_iban_exclusion_list(
    a_iban_country_code := 'AD',
    a_iban_national_id := '00020097',
    a_iban_bic := 'CRDAADADXXX'
) iban_exclusion_list_id;

SELECT payment.put_swift_routing_ssi(
    a_owner_bic := 'BINAADADXXX',
    a_owner_institution_name := 'MoraBanc',
    a_owner_institution_city := 'ANDORRA',
    a_owner_institution_country_code := 'AD',
    a_currency_code := 'EUR',
    a_asset_category := 'ANYY',
    a_correspondent_bic := 'BINAADADXXX',
    a_correspondent_institution_name := 'MoraBanc',
    a_correspondent_country_code := 'AD',
    a_correspondent_type := 'CORRESPONDENT'
) swift_routing_ssi_id;

-- IBAN validation
SELECT * FROM payment.validate_iban('BE88271080782541');

SELECT * FROM payment.is_valid_iban('GB82WEST12345698765432');
