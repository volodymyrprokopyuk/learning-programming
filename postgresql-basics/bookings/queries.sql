SELECT * FROM aircrafts_data;

-- LIKE must match string as a whole
SELECT *
FROM aircrafts_data
WHERE model->>'en' LIKE 'Airbus%';

SELECT *
FROM aircrafts_data
WHERE model->>'en' LIKE '%Superjet%';

SELECT *
FROM aircrafts_data
WHERE model->>'en' LIKE '%Caravan';

SELECT *
FROM airports_data
WHERE airport_name->>'en' LIKE '_a%';

-- Multiple conditions in WHERE
SELECT *
FROM aircrafts_data
WHERE model->>'en' NOT LIKE 'Airbus%' AND model->>'en' NOT LIkE 'Boeing%';

-- RegEx in WHERE
SELECT *
FROM aircrafts_data
WHERE model->>'en' ~ '^(Air|Boe)';

SELECT *
FROM aircrafts_data
WHERE model->>'en' !~ '\d+$';

-- Range in WHERE
SELECT *
FROM aircrafts_data
WHERE range BETWEEN 3000 AND 6000;

-- Compured attributes with alias
SELECT *, round(range / 1.609, 2) miles
FROM aircrafts_data;

-- Sort result by an attribute
SELECT *
FROM aircrafts_data
ORDER BY range DESC;

-- Remove duplicates
SELECT DISTINCT timezone
FROM airports_data;

-- LIMIT to X biggest by criteria
SELECT *
FROM airports_data
ORDER BY coordinates[0] DESC
LIMIT 3;

-- LIMIT to next X after the X biggest by criteria
SELECT *
FROM airports_data
ORDER BY coordinates[0] DESC
LIMIT 3 OFFSET 3;

-- CASE statement for categorization
SELECT model, range,
    CASE
        WHEN range < 2000 THEN 'Short distance'
        WHEN range < 5000 THEN 'Mid distance'
        ELSE 'Large distance'
    END aircraft_type
FROM aircrafts_data;
