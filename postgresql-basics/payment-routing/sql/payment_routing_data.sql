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

SELECT payment.put_iban_institution(
    a_institution_name := 'The Royal Bank of Scotland Plc, Belgium branch',
    a_institution_country_name := 'BELGIUM',
    a_institution_country_code := 'BE',
    a_iban_bic := 'ABNABEBRXXX',
    a_routing_bic := 'ABNABEBRXXX',
    a_iban_national_id := '271',
    a_iban_country_code := 'BE',
    a_is_sepa := true
) iban_institution_id;

-- IBAN exclusion list

SELECT payment.put_iban_exclusion_list(
    a_iban_country_code := 'AD',
    a_iban_national_id := '00020097',
    a_iban_bic := 'CRDAADADXXX'
) iban_exclusion_list_id;

-- SWIFT routing SSI

SELECT payment.put_routing_ssi(
    a_owner_bic := 'BINAADADXXX',
    a_owner_institution_name := 'MoraBanc',
    a_owner_institution_city := 'ANDORRA',
    a_owner_institution_country_code := 'AD',
    a_currency_code := 'EUR',
    a_asset_category := 'ANYY',
    a_correspondent_bic := 'BINAADADXXX',
    a_correspondent_institution_name := 'MoraBanc',
    a_correspondent_country_code := 'AD',
    a_correspondent_type := 'CORRESPONDENT',
    a_routing_ssi_source := 'SWIFT'
) routing_ssi_id;

SELECT payment.put_routing_ssi(
    a_owner_bic := 'ABNABEBRXXX',
    a_owner_institution_name := 'The Royal Bank of Scotland Plc, Belgium branch',
    a_owner_institution_city := 'BRUSSELS',
    a_owner_institution_country_code := 'BE',
    a_currency_code := 'EUR',
    a_asset_category := 'ANYY',
    a_correspondent_bic := 'ABNABEBRXXX',
    a_correspondent_institution_name := 'The Royal Bank of Scotland Plc, Belgium branch',
    a_correspondent_country_code := 'BE',
    a_correspondent_type := 'CORRESPONDENT',
    a_routing_ssi_source := 'SWIFT'
) routing_ssi_id;

-- PagoFX preferred direct correspondent

SELECT payment.put_routing_ssi(
    a_owner_bic := 'ABNABEBRXXX',
    a_owner_institution_name := 'The Royal Bank of Scotland Plc, Belgium branch',
    a_owner_institution_city := 'BRUSSELS',
    a_owner_institution_country_code := 'BE',
    a_currency_code := 'EUR',
    a_asset_category := 'ANYY',
    a_correspondent_bic := 'PAGOFXCOXXX',
    a_correspondent_institution_name := 'Preferred PagoFX correspondent',
    a_correspondent_country_code := 'BE',
    a_correspondent_type := 'CORRESPONDENT',
    a_routing_ssi_source := 'PAGOFX'
) routing_ssi_id;

-- SWIFT routing SSI correspondent chain

SELECT payment.put_routing_ssi(
    a_owner_bic := 'ABNABEBRXXX',
    a_owner_institution_name := 'The Royal Bank of Scotland Plc, Belgium branch',
    a_owner_institution_city := 'BRUSSELS',
    a_owner_institution_country_code := 'BE',
    a_currency_code := 'AED',
    a_asset_category := 'ANYY',
    a_correspondent_bic := 'CORRESPAXXX',
    a_correspondent_institution_name := 'Correspondent A',
    a_correspondent_country_code := 'ES',
    a_correspondent_type := 'LOCAL_CORRESPONDENT',
    a_routing_ssi_source := 'SWIFT'
) routing_ssi_id;

SELECT payment.put_routing_ssi(
    a_owner_bic := 'CORRESPAXXX',
    a_owner_institution_name := 'Correspondent A',
    a_owner_institution_city := 'MADRID',
    a_owner_institution_country_code := 'ES',
    a_currency_code := 'AED',
    a_asset_category := 'ANYY',
    a_correspondent_bic := 'CORRESPBXXX',
    a_correspondent_institution_name := 'Correspondent B',
    a_correspondent_country_code := 'UK',
    a_correspondent_type := 'LOCAL_CORRESPONDENT',
    a_routing_ssi_source := 'SWIFT'
) routing_ssi_id;

SELECT payment.put_routing_ssi(
    a_owner_bic := 'CORRESPBXXX',
    a_owner_institution_name := 'Correspondent B',
    a_owner_institution_city := 'LONDON',
    a_owner_institution_country_code := 'UK',
    a_currency_code := 'AED',
    a_asset_category := 'ANYY',
    a_correspondent_bic := 'CORRESPCXXX',
    a_correspondent_institution_name := 'Correspondent C',
    a_correspondent_country_code := 'AE',
    a_correspondent_type := 'CORRESPONDENT',
    a_routing_ssi_source := 'SWIFT'
) routing_ssi_id;

-- PAGOFX routing SSI correspondent chain

SELECT payment.put_routing_ssi(
    a_owner_bic := 'ABNABEBRXXX',
    a_owner_institution_name := 'The Royal Bank of Scotland Plc, Belgium branch',
    a_owner_institution_city := 'BRUSSELS',
    a_owner_institution_country_code := 'BE',
    a_currency_code := 'AED',
    a_asset_category := 'ANYY',
    a_correspondent_bic := 'CORRESPDXXX',
    a_correspondent_institution_name := 'Correspondent D',
    a_correspondent_country_code := 'FR',
    a_correspondent_type := 'LOCAL_CORRESPONDENT',
    a_routing_ssi_source := 'PAGOFX'
) routing_ssi_id;

SELECT payment.put_routing_ssi(
    a_owner_bic := 'CORRESPDXXX',
    a_owner_institution_name := 'Correspondent D',
    a_owner_institution_city := 'PARIS',
    a_owner_institution_country_code := 'FR',
    a_currency_code := 'AED',
    a_asset_category := 'ANYY',
    a_correspondent_bic := 'CORRESPEXXX',
    a_correspondent_institution_name := 'Correspondent E',
    a_correspondent_country_code := 'AE',
    a_correspondent_type := 'CORRESPONDENT',
    a_routing_ssi_source := 'PAGOFX'
) routing_ssi_id;

-- IBAN validation

SELECT * FROM payment.is_valid_iban('BE88271080782541');
SELECT * FROM payment.is_valid_iban('GB82WEST12345698765432');

SELECT iban, iban_national_id, account_number, iban_bic, is_sepa
FROM payment.validate_iban('BE88271080782541');

-- Get routing SSI

SELECT routing_ssi_id, owner_bic, currency_code,
    correspondent_bic, correspondent_type, routing_ssi_source
FROM payment.get_routing_ssi('ABNABEBRXXX', 'EUR');
SELECT routing_ssi_id, owner_bic, currency_code,
    correspondent_bic, correspondent_type, routing_ssi_source
FROM payment.get_routing_ssi('ABNABEBRXXX', 'AED');
