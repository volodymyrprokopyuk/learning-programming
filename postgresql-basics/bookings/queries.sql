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

-- IN in WHERE
SELECT *
FROM airports_data
WHERE city->>'en' IN ('Moscow', 'Ulyanovsk');

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

-- JOIN two relations on FOREIGN KEY
SELECT a.aircraft_code, a.model, s.seat_no, s.fare_conditions
FROM seats s
    JOIN aircrafts_data a ON a.aircraft_code = s.aircraft_code
WHERE a.model->>'en' ~ '^Cessna';

-- Combinatorics of all cities interconnections
-- by using cartesian product (CROSS JOIN) of airports against themselves
SELECT COUNT(*)
FROM airports_data fa, airports_data ta
WHERE fa.city <> ta.city;

-- Cities with more than one airport
SELECT a.city, COUNT(*)
FROM airports_data a
GROUP BY a.city
HAVING COUNT(*) > 1;

-- LEFT JOIN and GROUPing by attribute alias
SELECT a.aircraft_code, a.model, r.aircraft_code route_aircraft_code, COUNT(r.aircraft_code)
FROM aircrafts_data a
    LEFT JOIN routes r ON r.aircraft_code = a.aircraft_code
GROUP BY a.aircraft_code, a.model, route_aircraft_code
ORDER BY COUNT(r.aircraft_code) DESC;

-- Multiple JOINs. JOINs are left associative
SELECT COUNT(*)
FROM flights f
    JOIN ticket_flights t ON t.flight_id = f.flight_id
    LEFT JOIN boarding_passes b ON b.flight_id = t.flight_id AND b.ticket_no = t.ticket_no
WHERE f.actual_departure IS NOT NULL AND b.boarding_no IS NULL;

-- Amount ranges as VALUES virtual table
SELECT amount_range.min_amount, amount_range.max_amount, COUNT(b.*)
FROM bookings b
    RIGHT JOIN (VALUES
    -- amount_range virtual table definition
        (0, 100000), (100000, 200000),
        (200000, 300000), (300000, 400000),
        (400000, 500000), (500000, 600000),
        (600000, 700000), (700000, 800000),
        (800000, 900000), (900000, 1000000),
        (1000000, 1100000), (1100000, 1200000),
        (1200000, 1300000)
    -- amount_range virtual table declaration
    ) amount_range (min_amount, max_amount)
       ON b.total_amount >= amount_range.min_amount AND  b.total_amount < amount_range.max_amount
GROUP BY amount_range.min_amount, amount_range.max_amount
ORDER BY amount_range.min_amount

-- UNION, INTERSECT, EXCEPT
SELECT DISTINCT arrival_city
FROM routes
WHERE departure_city = 'Москва'
UNION -- INTERSECT, EXCEPT
SELECT DISTINCT arrival_city
FROM routes
WHERE departure_city = 'Санкт-Петербург'
