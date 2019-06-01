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
SELECT *, ROUND(range / 1.609, 2) miles
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

-- FROM t1, t2 (cartesian product) WHERE condition
-- Equivalent
-- FROM t1 JOIN t1 ON condition
SELECT COUNT(*)
FROM airports_data fa, airports_data ta
WHERE fa.airport_code <> ta.airport_code;
-- Equivalent
SELECT COUNT(*)
FROM airports_data fa
    JOIN airports_data ta ON fa.airport_code <> ta.airport_code;

-- LEFT JOIN and GROUPing by attribute alias
SELECT a.aircraft_code, a.model, r.aircraft_code route_aircraft_code, COUNT(r.aircraft_code)
FROM aircrafts_data a
    LEFT JOIN routes r ON r.aircraft_code = a.aircraft_code
GROUP BY a.aircraft_code, a.model, route_aircraft_code
ORDER BY COUNT(r.aircraft_code) DESC;

-- Number of seats per aircraft and per fare conditions
SELECT a.aircraft_code, a.model, s.fare_conditions, COUNT(s.*) seat_count, a.range
FROM aircrafts_data a
    JOIN seats s ON s.aircraft_code = a.aircraft_code
GROUP BY a.aircraft_code, s.fare_conditions
ORDER BY a.model, s.fare_conditions

-- Multiple JOINs. JOINs are left associative
SELECT COUNT(*)
FROM flights f
    JOIN ticket_flights t ON t.flight_id = f.flight_id
    LEFT JOIN boarding_passes b ON b.flight_id = t.flight_id AND b.ticket_no = t.ticket_no
WHERE f.actual_departure IS NOT NULL AND b.boarding_no IS NULL;

-- Amount ranges as VALUES virtual table
SELECT amount_range.min_amount, amount_range.max_amount, COUNT(b.*) booking_count
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

-- Aggragete functions: AVG, MIN, MAX
SELECT ROUND(AVG(total_amount), 2), MIN(total_amount), MAX(total_amount) FROM bookings;

-- GROUP BY and COUNT
SELECT arrival_city, COUNT(*)
FROM routes
WHERE departure_city = 'Москва'
GROUP BY arrival_city
ORDER BY COUNT(*) DESC;

SELECT ARRAY_LENGTH(days_of_week, 1) days_per_week, COUNT(*) number_of_routes
FROM routes
GROUP BY days_per_week
ORDER BY days_per_week DESC;

-- GROUP BY and HAVING
SELECT arrival_city, COUNT(*) number_of_routes
FROM routes
GROUP BY arrival_city
HAVING COUNT(*) >=15
ORDER BY COUNT(*) DESC;

-- ** WINDOW FUNCTION = aggregate() OVER **
-- WINDOW FUNCTION does not require GROUP BY clause
-- When GROUP BY is present in a query, then WINDOW FUNCTION is applied AFTER GROUP BY summarizing already grouped data
-- WINDOW FUNCTION periodically accumulates data within every PARTITION
-- WINDOW FUNCTION resets accumulator at the beginning of every PARTITION
-- PARTITION is a set of rows for which PARTITION BY <expression> gives the same value
-- WINDOW FUNCTION computes aggregate for the current row within a WINDOW FRAME of the current row
-- WINDOW FRAME for an unordered partition is PARTITION[begin..end]
-- WINDOW FRAME for an ordered partition is PARTITION[begin..current row]
-- Ordering within a PARTITION is defined by ORDER BY <expression>

SELECT b.book_ref, b.book_date,
    EXTRACT('month' FROM b.book_date) booking_month,
    EXTRACT('day' FROM b.book_date) booking_day,
    -- WINDOW FUNCTION
    COUNT(*) OVER (
        PARTITION BY DATE_TRUNC('month', b.book_date)
        ORDER BY b.book_date
    ) booking_count
FROM ticket_flights tf
    JOIN tickets t ON t.ticket_no = tf.ticket_no
    JOIN bookings b ON b.book_ref = t.book_ref
WHERE tf.flight_id = 1
ORDER BY b.book_date;

SELECT airport_name, city, timezone, coordinates,
    -- WINDOW FUNCTION
    RANK() OVER (PARTITION BY timezone ORDER BY coordinates[1] DESC)
FROM airports_data
ORDER BY timezone, rank;

-- WINDOW clause for defining named PARTITION
SELECT airport_name, city, timezone, coordinates,
    -- Named PARTITION usage
    RANK() OVER timezone_ordered_by_coordinates
FROM airports_data
-- Named PARTITION definition
WINDOW timezone_ordered_by_coordinates AS (PARTITION BY timezone ORDER BY coordinates[1] DESC)
ORDER BY timezone, rank;

-- Scalar subquery in WHERE
SELECT COUNT(*)
FROM bookings
WHERE total_amount > (SELECT AVG(total_amount) FROM bookings);

-- Uncorrelated (only one time per outer query) subquery in WHERE
SELECT flight_no, departure_city, arrival_city
FROM routes
WHERE departure_city IN (SELECT city->>'ru' FROM airports_data WHERE timezone ~ 'Krasnoyarsk')
    AND arrival_city IN (SELECT city->>'ru' FROM airports_data WHERE timezone ~ 'Krasnoyarsk');

-- Uncorrelated subquery in WHERE with IN/NOT IN predicate
SELECT airport_name, city, coordinates
FROM airports_data
WHERE coordinates[0] IN (
    (SELECT MAX(coordinates[0]) FROM airports_data),
    (SELECT MIN(coordinates[0]) FROM airports_data)
);

-- Correlated (subquery per every row in outer query) subquery in WHERE
-- with EXISTS/NOT EXISTS predicate
SELECT DISTINCT a.city
FROM airports_data a
WHERE NOT EXISTS (
    SELECT 1
    FROM routes r
    -- Correlated subquery (subquery has a reference to the outer query)
    WHERE r.departure_city = 'Москва' AND r.arrival_city = a.city->>'ru'
) AND a.city->>'ru' <> 'Москва'
ORDER BY a.city

-- Correlated subquery in SELECT
SELECT a.model, (
    SELECT COUNT(*)
    FROM seats s
    WHERE s.aircraft_code = a.aircraft_code AND s.fare_conditions = 'Business'
) business, (
    SELECT COUNT(*)
    FROM seats s
    WHERE s.aircraft_code = a.aircraft_code AND s.fare_conditions = 'Comfort'
) comfort, (
    SELECT COUNT(*)
    FROM seats s
    WHERE s.aircraft_code = a.aircraft_code AND s.fare_conditions = 'Economy'
) economy
FROM aircrafts_data a
ORDER BY economy DESC;

-- Correlated subquery in FROM
SELECT sc.model, STRING_AGG(sc.fare_conditions || ' ' || sc.seats_count, ', ') seats_count
FROM (
    SELECT a.model, s.fare_conditions, count(*) seats_count
    FROM aircrafts_data a JOIN seats s ON s.aircraft_code = a.aircraft_code
    GROUP BY a.model, s.fare_conditions
) sc
GROUP BY sc.model

-- Uncorrelated subquery in FROM
SELECT a.city, a.airport_code, a.airport_name, multiple_airport_city.airport_count
FROM (
    SELECT city, COUNT(*) airport_count
    FROM airports_data
    GROUP BY city
    HAVING COUNT(*) > 1
) multiple_airport_city
     JOIN airports_data a ON a.city = multiple_airport_city.city

-- Uncorrelated subquery in HAVING
SELECT departure_city, departure_airport, COUNT(*) route_count
FROM routes
GROUP BY departure_airport, departure_city
HAVING departure_airport IN (
    SELECT airport_code
    FROM airports_data
    WHERE coordinates[0] > 150
)

-- Nested subqueries
SELECT ts.flight_id, ts.flight_no, ac.model->>'ru' aircraft_model,
    dep.airport_name->>'ru' departure_airport, ts.scheduled_departure,
     arr.airport_name->>'ru' arrival_airport,
    ts.ticket_count, ts.seat_count,
    ROUND(ticket_count::numeric / seat_count::numeric, 2) aircraft_usage
FROM (
    SELECT f.flight_id, f.flight_no, f.aircraft_code,
        f.departure_airport, f.scheduled_departure, f.arrival_airport,
        COUNT(tf.ticket_no) ticket_count, (
            SELECT COUNT(s.seat_no) FROM seats s WHERE s.aircraft_code = f.aircraft_code
        ) seat_count
    FROM flights f
        JOIN ticket_flights tf ON tf.flight_id = f.flight_id
    WHERE f.status = 'Arrived'
    GROUP BY f.flight_id, f.flight_no, f.aircraft_code,
        f.departure_airport, f.scheduled_departure, f.arrival_airport
) ts
    JOIN aircrafts_data ac ON ac.aircraft_code = ts.aircraft_code
    JOIN airports_data dep ON dep.airport_code = ts.departure_airport
    JOIN airports_data arr ON arr.airport_code = ts.arrival_airport
ORDER BY ts.flight_no, ts.scheduled_departure
LIMIT 20

-- Common Table Expression (CTE) builds temporary table
WITH ticket_seat AS (
    SELECT f.flight_id, f.flight_no, f.aircraft_code,
        f.departure_airport, f.scheduled_departure, f.arrival_airport,
        COUNT(tf.ticket_no) ticket_count, (
            SELECT COUNT(s.seat_no) FROM seats s WHERE s.aircraft_code = f.aircraft_code
        ) seat_count
    FROM flights f
        JOIN ticket_flights tf ON tf.flight_id = f.flight_id
    WHERE f.status = 'Arrived'
    GROUP BY f.flight_id, f.flight_no, f.aircraft_code,
        f.departure_airport, f.scheduled_departure, f.arrival_airport
)
SELECT ts.flight_id, ts.flight_no, ac.model->>'ru' aircraft_model,
    dep.airport_name->>'ru' departure_airport, ts.scheduled_departure,
     arr.airport_name->>'ru' arrival_airport,
    ts.ticket_count, ts.seat_count,
    ROUND(ticket_count::numeric / seat_count::numeric, 2) aircraft_usage
FROM ticket_seat ts
    JOIN aircrafts_data ac ON ac.aircraft_code = ts.aircraft_code
    JOIN airports_data dep ON dep.airport_code = ts.departure_airport
    JOIN airports_data arr ON arr.airport_code = ts.arrival_airport
ORDER BY ts.flight_no, ts.scheduled_departure
LIMIT 20

-- WITH RECURSIVE
-- Virtual table declaration
WITH RECURSIVE amount_range (min_amount, max_amount) AS (
    -- Virtual table recursive definition
    -- Initial values
    VALUES (0, 100000)
    UNION ALL
    -- SELECT agains last selected row
    SELECT min_amount + 100000, max_amount + 100000
    FROM amount_range
    -- Recursion stop condition
    WHERE max_amount < (SELECT MAX(total_amount) FROM bookings)
)
SELECT *
FROM amount_range;

WITH RECURSIVE amount_range (min_amount, max_amount) AS (
    VALUES (0, 100000)
    UNION ALL
    SELECT min_amount + 100000, max_amount + 100000
    FROM amount_range
    WHERE max_amount < (SELECT MAX(total_amount) FROM bookings)
)
SELECT r.min_amount, r.max_amount, COUNT(b.*) booking_count
FROM bookings b
    RIGHT JOIN amount_range r
        ON b.total_amount >= r.min_amount AND b.total_amount < r.max_amount
GROUP BY r.min_amount, r.max_amount
ORDER BY r.min_amount

-- routes from flight history query
WITH all_flight AS (
    SELECT f.flight_no, f.aircraft_code, f.departure_airport, f.arrival_airport,
        (f.scheduled_arrival - scheduled_departure) duration,
        TO_CHAR(f.scheduled_departure, 'ID'::text)::integer day_of_week
    FROM flights f
),
single_flight_per_day_of_week AS (
    SELECT af.flight_no, af.aircraft_code, af.departure_airport, af.arrival_airport,
        af.duration, af.day_of_week
    FROM all_flight af
    GROUP BY 1, 2, 3, 4, 5, 6
    ORDER BY 1, 2, 3, 4, 5, 6
),
single_flight_per_week AS (
    SELECT fdw.flight_no, fdw.aircraft_code, fdw.departure_airport, fdw.arrival_airport,
        fdw.duration, ARRAY_AGG(fdw.day_of_week) days_of_week
    FROM single_flight_per_day_of_week fdw
    GROUP BY 1, 2, 3, 4, 5
)
SELECT fw.flight_no, fw.aircraft_code,
    dep.city->>'ru' dep_city, fw.departure_airport, dep.airport_name->>'ru' dep_airpot_name,
    arr.city->>'ru' arr_city, fw.arrival_airport, arr.airport_name->>'ru' arr_airport_name,
    fw.duration, fw.days_of_week
FROM single_flight_per_week fw
    JOIN airports_data dep ON dep.airport_code = fw.departure_airport
    JOIN airports_data arr ON arr.airport_code = fw.arrival_airport
