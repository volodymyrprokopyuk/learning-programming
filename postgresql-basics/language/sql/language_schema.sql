CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE SCHEMA customer;

-- Data

CREATE TYPE customer.language_code_t AS
    ENUM ('en', 'es', 'fr', 'nl');

CREATE TYPE customer.language_purpose_t AS
    ENUM (
        'default',
        'app_localization_default',
        'app_localization',
        'marketing_communication_default',
        'marketing_communication',
        'customer_support_default',
        'customer_support'
    );

CREATE TYPE customer.language_context_t AS
    ENUM ('uk', 'be');

CREATE TABLE customer.customer (
    customer_id uuid NOT NULL
        DEFAULT gen_random_uuid(),
    email varchar(50) NOT NULL,
    CONSTRAINT pk_customer
        PRIMARY KEY (customer_id)
);

CREATE TABLE customer.supported_language (
    supported_language_id uuid NOT NULL
        DEFAULT gen_random_uuid(),
    language_code customer.language_code_t NOT NULL,
    language_name varchar(50) NOT NULL,
    purpose customer.language_purpose_t NOT NULL,
    context customer.language_context_t NOT NULL,
    priority integer NOT NULL,
    CONSTRAINT pk_supported_language
        PRIMARY KEY (supported_language_id),
    CONSTRAINT uq_language_purpose_context
        UNIQUE (language_code, purpose, context)
);

CREATE TABLE customer.language_configuration (
    language_configuration_id uuid NOT NULL
        DEFAULT gen_random_uuid(),
    customer_id uuid NOT NULL,
    app_localization_language_id uuid NOT NULL,
    marketing_communication_language_id uuid NOT NULL,
    customer_support_language_id uuid NOT NULL,
    CONSTRAINT pk_language_configuration
        PRIMARY KEY (language_configuration_id),
    CONSTRAINT fk_language_configuration_belongs_to_customer
        FOREIGN KEY (customer_id) REFERENCES customer.customer (customer_id)
        ON UPDATE CASCADE ON DELETE CASCADE,
    CONSTRAINT uq_there_is_only_on_language_configuration_per_customer
        UNIQUE (customer_id),
    CONSTRAINT fk_language_configuration_has_app_localization_language
        FOREIGN KEY (app_localization_language_id)
        REFERENCES customer.supported_language (supported_language_id)
        ON UPDATE CASCADE ON DELETE CASCADE,
    CONSTRAINT fk_language_configuration_has_marketing_communication_language
        FOREIGN KEY (marketing_communication_language_id)
        REFERENCES customer.supported_language (supported_language_id)
        ON UPDATE CASCADE ON DELETE CASCADE,
    CONSTRAINT fk_language_configuration_has_customer_support_language
        FOREIGN KEY (customer_support_language_id)
        REFERENCES customer.supported_language (supported_language_id)
        ON UPDATE CASCADE ON DELETE CASCADE
);

-- Interface

CREATE OR REPLACE FUNCTION customer.put_customer(
    a_email varchar(50),
    a_customer_id uuid DEFAULT NULL
) RETURNS uuid
LANGUAGE sql AS $$
INSERT INTO customer.customer (customer_id, email)
    VALUES (coalesce(a_customer_id, gen_random_uuid()), a_email)
ON CONFLICT ON CONSTRAINT pk_customer DO UPDATE SET
    email = excluded.email
RETURNING customer_id;
$$;

CREATE OR REPLACE FUNCTION customer.put_supported_langauge(
    a_language_code customer.language_code_t,
    a_language_name varchar(50),
    a_purpose customer.language_purpose_t,
    a_context customer.language_context_t,
    a_priority integer,
    a_supported_language_id uuid DEFAULT NULL
) RETURNS uuid
LANGUAGE sql AS $$
INSERT INTO customer.supported_language (
    supported_language_id, language_code, language_name, purpose, context, priority
) VALUES (
    coalesce(a_supported_language_id, gen_random_uuid()),
    a_language_code, a_language_name, a_purpose, a_context, a_priority
) ON CONFLICT ON CONSTRAINT pk_supported_language DO UPDATE SET
    language_code = excluded.language_code,
    language_name = excluded.language_name,
    purpose = excluded.purpose,
    context = excluded.context,
    priority = excluded.priority
RETURNING supported_language_id;
$$;

CREATE OR REPLACE FUNCTION customer.get_language_or_default(
    a_language_code customer.language_code_t,
    a_purpose customer.language_purpose_t,
    a_default_purposes customer.language_purpose_t[],
    a_context customer.language_context_t
) RETURNS TABLE (
    supported_language_id uuid,
    language_code customer.language_code_t,
    language_name varchar(50),
    purpose customer.language_purpose_t,
    context customer.language_context_t,
    priority integer
) LANGUAGE sql AS $$
WITH specific_language AS (
    SELECT sp.supported_language_id, sp.language_code, sp.language_name,
        sp.purpose, sp.context, sp.priority
    FROM customer.supported_language sp
    WHERE sp.language_code = a_language_code
        AND sp.purpose = a_purpose AND sp.context = a_context
)
SELECT sl.supported_language_id, sl.language_code, sl.language_name,
    sl.purpose, sl.context, sl.priority
FROM specific_language sl
UNION
SELECT dl.supported_language_id, dl.language_code, dl.language_name,
    dl.purpose, dl.context, dl.priority
FROM customer.supported_language dl
WHERE NOT EXISTS (SELECT 1 FROM specific_language)
    AND dl.purpose = ANY(a_default_purposes)
    AND dl.context = a_context
ORDER BY priority
LIMIT 1;
$$;

CREATE OR REPLACE FUNCTION customer.put_langauge_configuration(
    a_customer_id uuid,
    a_context customer.language_context_t,
    a_app_localization_language_code customer.language_code_t,
    a_marketing_communication_language_code customer.language_code_t,
    a_customer_support_language_code customer.language_code_t
) RETURNS uuid
LANGUAGE sql AS $$
INSERT INTO customer.language_configuration (
    customer_id,
    app_localization_language_id,
    marketing_communication_language_id,
    customer_support_language_id
) VALUES (
    a_customer_id,
    (SELECT l.supported_language_id
    FROM customer.get_language_or_default(
        a_app_localization_language_code, 'app_localization',
        ARRAY ['app_localization', 'app_localization_default', 'default']
            ::customer.language_purpose_t[], a_context
    ) l),
    (SELECT l.supported_language_id
    FROM customer.get_language_or_default(
        a_marketing_communication_language_code, 'marketing_communication',
        ARRAY ['marketing_communication', 'marketing_communication_default', 'default']
            ::customer.language_purpose_t[], a_context
    ) l),
    (SELECT l.supported_language_id
    FROM customer.get_language_or_default(
        a_customer_support_language_code, 'customer_support',
        ARRAY ['customer_support', 'customer_support_default', 'default']
            ::customer.language_purpose_t[], a_context
    ) l)
) ON CONFLICT ON CONSTRAINT uq_there_is_only_on_language_configuration_per_customer
DO UPDATE SET
    app_localization_language_id = excluded.app_localization_language_id,
    marketing_communication_language_id = excluded.marketing_communication_language_id,
    customer_support_language_id = excluded.customer_support_language_id
RETURNING language_configuration_id;
$$;

CREATE OR REPLACE FUNCTION customer.get_language_configuration(
    a_customer_id uuid
) RETURNS TABLE (
    customer_id uuid,
    app_localization_language_code customer.language_code_t,
    app_localization_language_name varchar(50),
    marketing_comunication_language_code customer.language_code_t,
    marketing_comunication_language_name varchar(50),
    customer_support_language_code customer.language_code_t,
    customer_support_language_name varchar(50)
) LANGUAGE sql AS $$
SELECT a_customer_id,
    al.language_code, al.language_name,
    mc.language_code, mc.language_name,
    cs.language_code, cs.language_name
FROM customer.language_configuration lc
    JOIN customer.supported_language al
        ON al.supported_language_id = lc.app_localization_language_id
    JOIN customer.supported_language mc
        ON mc.supported_language_id = lc.marketing_communication_language_id
    JOIN customer.supported_language cs
        ON cs.supported_language_id = lc.customer_support_language_id
WHERE lc.customer_id = a_customer_id;
$$;
