-- Customer

SELECT customer.put_customer('bob@mail.com', '2e7c3f73-0d93-41fa-802a-63a178b85366');
SELECT customer.put_customer('alice@mail.com', 'bf69407f-9800-48ef-b44b-201511ce29f5');

-- Supported langauge

-- UK context

SELECT customer.put_supported_langauge(
    'en', 'English', 'default', 'uk', 100,
    '289c754c-bda2-4487-900a-fd46e2af42e8'
);
SELECT customer.put_supported_langauge(
    'es', 'Español', 'app_localization', 'uk', 10,
    'd940c26b-ca60-4fe7-bde3-c9a9612608d9'
);
SELECT customer.put_supported_langauge(
    'es', 'Español', 'marketing_communication', 'uk', 10,
    '76804e55-c52e-4736-a8ae-e6c34bb3ff80'
);

-- BE context

SELECT customer.put_supported_langauge(
    'fr', 'Français', 'default', 'be', 100,
    '8b797d9c-6431-4b50-8210-99c8da48bb7c'
);
SELECT customer.put_supported_langauge(
    'en', 'English', 'app_localization', 'be', 20,
    '75b58806-e491-4b39-9590-85fbf405e36c'
);
SELECT customer.put_supported_langauge(
    'nl', 'Nederlands', 'app_localization', 'be', 10,
    '1e636e60-c052-4487-b5a1-4f517c0d35c5'
);
SELECT customer.put_supported_langauge(
    'en', 'English', 'marketing_communication', 'be', 20,
    '5794b790-a357-468a-b28d-8ccae79806e9'
);
SELECT customer.put_supported_langauge(
    'nl', 'Nederlands', 'marketing_communication', 'be', 10,
    '5aee4b89-8946-4d20-bac2-ac8f87f1679e'
);

-- Language configuration

-- Bob in UK

SELECT customer.put_langauge_configuration(
    '2e7c3f73-0d93-41fa-802a-63a178b85366', 'uk', 'es', 'es', 'es'
);

SELECT app_localization_language_code app_localization,
    marketing_comunication_language_code marketing_communication,
    customer_support_language_code customer_support
FROM customer.get_language_configuration('2e7c3f73-0d93-41fa-802a-63a178b85366');

-- Alice in BE

SELECT customer.put_langauge_configuration(
    'bf69407f-9800-48ef-b44b-201511ce29f5', 'be', 'es', 'en', 'es'
);

SELECT app_localization_language_code app_localization,
    marketing_comunication_language_code marketing_communication,
    customer_support_language_code customer_support
FROM customer.get_language_configuration('bf69407f-9800-48ef-b44b-201511ce29f5');
