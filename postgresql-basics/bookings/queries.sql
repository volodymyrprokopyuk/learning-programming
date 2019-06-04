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

-- IN for multiple attributes
SELECT *
FROM flights
WHERE (departure_airport, arrival_airport) IN (('DME', 'BTK'), ('VKO', 'HMA'));

SELECT *
FROM airports_data
WHERE timezone IN ('Asia/Novokuznetsk', 'Asia/Krasnoyarsk');
-- Equivalent
SELECT *
FROM airports_data
WHERE timezone = ANY (VALUES ('Asia/Novokuznetsk'), ('Asia/Krasnoyarsk'));

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

-- regexp_match
SELECT (regexp_match(passenger_name, '^(\w+)'))[1] first_name, count(*) first_name_count
FROM tickets
GROUP BY first_name
ORDER BY first_name_count DESC

-- JOIN two relations on FOREIGN KEY
SELECT a.aircraft_code, a.model, s.seat_no, s.fare_conditions
FROM seats s
    JOIN aircrafts_data a ON a.aircraft_code = s.aircraft_code
WHERE a.model->>'en' ~ '^Cessna';

SELECT r.*, a.model->>'ru'
FROM routes r
    JOIN aircrafts_data a ON a.aircraft_code = r.aircraft_code
WHERE r.aircraft_code = '733'

SELECT f.departure_city, f.arrival_city, min(tf.amount), max(tf.amount)
FROM flights_v f
    LEFT JOIN ticket_flights tf ON tf.flight_id = f.flight_id
GROUP BY f.departure_city, f.arrival_city
ORDER BY f.departure_city, f.arrival_city

-- Combinatorics of all cities interconnections
-- by using cartesian product (CROSS JOIN) of airports against themselves
SELECT count(*)
FROM airports_data fa, airports_data ta
WHERE fa.city <> ta.city;

-- Cities with more than one airport
SELECT a.city, count(*)
FROM airports_data a
GROUP BY a.city
HAVING count(*) > 1;

-- FROM t1, t2 (cartesian product) WHERE condition
-- Equivalent
-- FROM t1 JOIN t1 ON condition
SELECT count(*)
FROM airports_data fa, airports_data ta
WHERE fa.airport_code <> ta.airport_code;
-- Equivalent
SELECT count(*)
FROM airports_data fa
    JOIN airports_data ta ON fa.airport_code <> ta.airport_code;

-- LEFT JOIN and GROUPing by attribute alias
SELECT a.aircraft_code, a.model, r.aircraft_code route_aircraft_code, count(r.aircraft_code)
FROM aircrafts_data a
    LEFT JOIN routes r ON r.aircraft_code = a.aircraft_code
GROUP BY a.aircraft_code, a.model, route_aircraft_code
ORDER BY count(r.aircraft_code) DESC;

-- Number of seats per aircraft and per fare conditions
SELECT a.aircraft_code, a.model, s.fare_conditions, count(s.*) seat_count, a.range
FROM aircrafts_data a
    JOIN seats s ON s.aircraft_code = a.aircraft_code
GROUP BY a.aircraft_code, s.fare_conditions
ORDER BY a.model, s.fare_conditions

-- Multiple JOINs. JOINs are left associative
SELECT count(*)
FROM flights f
    JOIN ticket_flights t ON t.flight_id = f.flight_id
    LEFT JOIN boarding_passes b ON b.flight_id = t.flight_id AND b.ticket_no = t.ticket_no
WHERE f.actual_departure IS NOT NULL AND b.boarding_no IS NULL;

-- Amount ranges as VALUES virtual table
SELECT amount_range.min_amount, amount_range.max_amount, count(b.*) booking_count
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

-- FILTER in aggragete()
SELECT (regexp_match(passenger_name, '^(\w+)'))[1] first_name,
    count(*) FILTER (WHERE passenger_name ~ '^ALEKSANDRA') first_name_count
FROM tickets
WHERE passenger_name ~ '^ALEKS'
GROUP BY first_name
ORDER BY first_name_count DESC

SELECT a.model, s.fare_conditions, count(s.*)
FROM aircrafts_data a
    JOIN seats s ON s.aircraft_code = a.aircraft_code
GROUP BY a.model, s.fare_conditions;

-- UNION, INTERSECT, EXCEPT
SELECT DISTINCT arrival_city
FROM routes
WHERE departure_city = 'Москва'
UNION -- INTERSECT, EXCEPT
SELECT DISTINCT arrival_city
FROM routes
WHERE departure_city = 'Санкт-Петербург'

-- Aggragete functions: avg, min, max
SELECT round(avg(total_amount), 2), min(total_amount), max(total_amount) FROM bookings;

-- GROUP BY and count
SELECT arrival_city, count(*)
FROM routes
WHERE departure_city = 'Москва'
GROUP BY arrival_city
ORDER BY count(*) DESC;

SELECT array_length(days_of_week, 1) days_per_week, count(*) number_of_routes
FROM routes
GROUP BY days_per_week
ORDER BY days_per_week DESC;

SELECT r.departure_city, r.arrival_city, count(*)
FROM routes r
WHERE r.departure_city = 'Москва'
    AND r.arrival_city = 'Санкт-Петербург'
GROUP BY r.departure_city, r.arrival_city

SELECT r.departure_city, count(DISTINCT r.arrival_city) routes_count
FROM routes r
GROUP BY r.departure_city
ORDER BY routes_count DESC

SELECT r.departure_city, r.arrival_city, count(r.arrival_city) route_count, r.days_of_week
FROM routes r
WHERE r.departure_city = 'Москва' AND array_length(r.days_of_week, 1) = 7
GROUP BY r.arrival_city, r.departure_city, r.days_of_week
HAVING count(r.arrival_city) > 1
ORDER BY route_count DESC

SELECT r.departure_city, unnest(r.days_of_week) day_of_week,
    count(r.departure_city) route_count
FROM routes r
WHERE r.departure_city = 'Москва'
GROUP BY day_of_week, r.departure_city
ORDER BY route_count DESC

-- unnest a single array into a relation
SELECT unnest('{1, 2, 3, 4, 5, 6, 7}'::integer[]);

-- unnset multiple arrays into a relation (only allowed in FROM clause)
SELECT *
FROM unnest(
    '{1, 2, 3, 4, 5, 6, 7}'::integer[],
    '{"Пн", "Вт", "Ср", "Чт", "Пт", "Сб", "Нд"}'::text[]
) week_days(num_of_day, name_of_day);

-- WITH ORDINALITY instread of explicit integer[]
SELECT *
FROM unnest(
    '{"Пн", "Вт", "Ср", "Чт", "Пт", "Сб", "Нд"}'::text[]
) WITH ORDINALITY week_days(name_of_day, num_of_day);

WITH week_days(num_of_day, name_of_day) AS (
    SELECT *
    FROM unnest(
        '{1, 2, 3, 4, 5, 6, 7}'::integer[],
        '{"Пн", "Вт", "Ср", "Чт", "Пт", "Сб", "Нд"}'::text[]
    )
),
routes_from AS (
    SELECT r.departure_city, unnest(r.days_of_week) day_of_week,
        count(r.departure_city) route_count
    FROM routes r
    WHERE r.departure_city = 'Москва'
    GROUP BY day_of_week, r.departure_city
    ORDER BY route_count DESC
)
SELECT rf.departure_city, rf.day_of_week, wd.name_of_day, rf.route_count
FROM routes_from rf
    JOIN week_days wd ON wd.num_of_day = rf.day_of_week;

-- Remove rows with equivalent swapped values
SELECT
    least(r.departure_city, r.arrival_city),
    greatest(r.departure_city, r.arrival_city)
FROM routes r
WHERE r.aircraft_code = '773'
GROUP BY
    least(r.departure_city, r.arrival_city),
    greatest(r.departure_city, r.arrival_city)

SELECT a.model, count(r.*) route_count,
    round(count(r.*)::numeric / (SELECT count(*) from routes)::numeric, 3)
FROM routes r
    RIGHT JOIN aircrafts_data a ON a.aircraft_code = r.aircraft_code
GROUP BY a.model
ORDER BY route_count DESC

-- All combinations of all cities
WITH cities AS (
    SELECT DISTINCT city FROM airports_data
)
SELECT count(*)
FROM cities dep, cities arr
WHERE dep.city <> arr.city

-- GROUP BY and HAVING
SELECT arrival_city, count(*) number_of_routes
FROM routes
GROUP BY arrival_city
HAVING count(*) >=15
ORDER BY count(*) DESC;

-- ** WINDOW FUNCTION = aggregate() OVER (PARTITION BY <expression> ORDER BY <column>) **
-- WINDOW FUNCTION does not require GROUP BY clause
-- When GROUP BY is present in a query, then WINDOW FUNCTION is applied AFTER GROUP BY
-- summarizing already grouped data
-- WINDOW FUNCTION periodically accumulates data within every PARTITION
-- WINDOW FUNCTION resets accumulator at the beginning of every PARTITION
-- PARTITION is a set of rows for which PARTITION BY <expression> gives the same value
-- WINDOW FUNCTION computes aggregate for the current row within a WINDOW FRAME
-- of the current row
-- WINDOW FRAME for an unordered partition is PARTITION[begin..end] (entire PARTITION)
-- WINDOW FRAME for an ordered partition is PARTITION[begin..current row] (growing frame)
-- Ordering within a PARTITION is defined by ORDER BY <expression>

SELECT b.book_ref, b.book_date,
    extract('month' FROM b.book_date) booking_month,
    extract('day' FROM b.book_date) booking_day,
    -- WINDOW FUNCTION
    count(*) OVER (
        PARTITION BY date_trunc('month', b.book_date)
        ORDER BY b.book_date
    ) booking_count
FROM ticket_flights tf
    JOIN tickets t ON t.ticket_no = tf.ticket_no
    JOIN bookings b ON b.book_ref = t.book_ref
WHERE tf.flight_id = 1
ORDER BY b.book_date;

SELECT airport_name, city, timezone, coordinates,
    -- WINDOW FUNCTION
    rank() OVER (PARTITION BY timezone ORDER BY coordinates[1] DESC)
FROM airports_data
ORDER BY timezone, rank;

-- WINDOW clause for defining named PARTITION
SELECT airport_name, city, timezone, coordinates,
    -- Named PARTITION usage
    rank() OVER timezone_ordered_by_coordinates
FROM airports_data
-- Named PARTITION definition
WINDOW timezone_ordered_by_coordinates AS (PARTITION BY timezone ORDER BY coordinates[1] DESC)
ORDER BY timezone, rank;

-- rank by aircraft range
SELECT *, rank() OVER (PARTITION BY  left(model->>'en', 6) ORDER BY range DESC)
FROM aircrafts_data
WHERE model->>'en' ~ '^(Airbus|Boeing)';

-- count seats per model using WINDOW FUNCTION
SELECT DISTINCT a.model, count(*) OVER (PARTITION BY a.model) seat_count
FROM aircrafts_data a
    JOIN seats s ON s.aircraft_code = a.aircraft_code

-- count seats per model using GROUP BY
SELECT a.model, count(*) seat_count
FROM aircrafts_data a
    JOIN seats s ON s.aircraft_code = a.aircraft_code
GROUP BY a.model

-- rank aircraft seats per aircraft model
WITH aircraft_seat AS (
    SELECT a.model, count(s.*) seat_count
    FROM aircrafts_data a
        JOIN seats s ON s.aircraft_code = a.aircraft_code
    GROUP BY a.model
)
SELECT acs.*, rank() OVER (PARTITION BY left(acs.model->>'en', 6) ORDER BY seat_count)
FROM aircraft_seat acs

-- Scalar subquery in WHERE
SELECT count(*)
FROM bookings
WHERE total_amount > (SELECT avg(total_amount) FROM bookings);

-- Uncorrelated (only one time per outer query) subquery in WHERE
SELECT flight_no, departure_city, arrival_city
FROM routes
WHERE departure_city IN (SELECT city->>'ru' FROM airports_data WHERE timezone ~ 'Krasnoyarsk')
    AND arrival_city IN (SELECT city->>'ru' FROM airports_data WHERE timezone ~ 'Krasnoyarsk');

-- Uncorrelated subquery in WHERE with IN/NOT IN predicate
SELECT airport_name, city, coordinates
FROM airports_data
WHERE coordinates[0] IN (
    (SELECT max(coordinates[0]) FROM airports_data),
    (SELECT min(coordinates[0]) FROM airports_data)
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
    SELECT count(*)
    FROM seats s
    WHERE s.aircraft_code = a.aircraft_code AND s.fare_conditions = 'Business'
) business, (
    SELECT count(*)
    FROM seats s
    WHERE s.aircraft_code = a.aircraft_code AND s.fare_conditions = 'Comfort'
) comfort, (
    SELECT count(*)
    FROM seats s
    WHERE s.aircraft_code = a.aircraft_code AND s.fare_conditions = 'Economy'
) economy
FROM aircrafts_data a
ORDER BY economy DESC;

-- Correlated subquery in FROM
SELECT sc.model, string_agg(sc.fare_conditions || ' ' || sc.seats_count, ', ') seats_count
FROM (
    SELECT a.model, s.fare_conditions, count(*) seats_count
    FROM aircrafts_data a JOIN seats s ON s.aircraft_code = a.aircraft_code
    GROUP BY a.model, s.fare_conditions
) sc
GROUP BY sc.model

-- Uncorrelated subquery in FROM
SELECT a.city, a.airport_code, a.airport_name, multiple_airport_city.airport_count
FROM (
    SELECT city, count(*) airport_count
    FROM airports_data
    GROUP BY city
    HAVING count(*) > 1
) multiple_airport_city
     JOIN airports_data a ON a.city = multiple_airport_city.city

-- Uncorrelated subquery in HAVING
SELECT departure_city, departure_airport, count(*) route_count
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
    round(ticket_count::numeric / seat_count::numeric, 2) aircraft_usage
FROM (
    SELECT f.flight_id, f.flight_no, f.aircraft_code,
        f.departure_airport, f.scheduled_departure, f.arrival_airport,
        count(tf.ticket_no) ticket_count, (
            SELECT count(s.seat_no) FROM seats s WHERE s.aircraft_code = f.aircraft_code
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
        count(tf.ticket_no) ticket_count, (
            SELECT count(s.seat_no) FROM seats s WHERE s.aircraft_code = f.aircraft_code
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
    round(ticket_count::numeric / seat_count::numeric, 2) aircraft_usage
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
    -- SELECT the last row created in previous recursive interation
    SELECT min_amount + 100000, max_amount + 100000
    FROM amount_range
    -- Recursion stop condition
    WHERE max_amount < (SELECT max(total_amount) FROM bookings)
)
SELECT *
FROM amount_range;

WITH RECURSIVE amount_range (min_amount, max_amount) AS (
    VALUES (0, 100000)
    UNION ALL
    SELECT min_amount + 100000, max_amount + 100000
    FROM amount_range
    WHERE max_amount < (SELECT max(total_amount) FROM bookings)
)
SELECT r.min_amount, r.max_amount, count(b.*) booking_count
FROM bookings b
    RIGHT JOIN amount_range r
        ON b.total_amount >= r.min_amount AND b.total_amount < r.max_amount
GROUP BY r.min_amount, r.max_amount
ORDER BY r.min_amount

-- Automatic generation of aircraft seats from aircraft_config and fare_conditions
WITH RECURSIVE
aircraft_config AS (
    SELECT *
    FROM (VALUES
        ('Airbus 2 5 D', 2, 5, 'D'),
        ('Boeing 3 7 F', 3, 7, 'F')
    ) aircraft_config(aircraft_code, max_seat_business, max_seat_economy, max_letter)
),
fare_conditions AS (
    SELECT *
    FROM (VALUES ('Business'), ('Economy')) fare_conditions(fare_condition)
),
seat_letters AS (
    SELECT *
    FROM (VALUES ('A'), ('B'), ('C'), ('D'), ('E'), ('F')) seat_letters(seat_letter)
),
-- The only recursive query
seat_numbers(seat_number) AS (
    VALUES (1)
    UNION ALL
    SELECT seat_number + 1 FROM seat_numbers WHERE seat_number < 10
)
SELECT ac.aircraft_code, fc.fare_condition, sl.seat_letter, sn.seat_number
FROM aircraft_config ac, fare_conditions fc, seat_letters sl, seat_numbers sn
WHERE sl.seat_letter <= ac.max_letter
    AND CASE
        WHEN fc.fare_condition = 'Business' THEN
            sn.seat_number <= ac.max_seat_business
        WHEN fc.fare_condition = 'Economy' THEN
            sn.seat_number > ac.max_seat_business
            AND sn.seat_number <= ac.max_seat_economy
    END
ORDER BY ac.aircraft_code, fc.fare_condition, sn.seat_number, sl.seat_letter

-- routes from flight history query
WITH all_flight AS (
    SELECT f.flight_no, f.aircraft_code, f.departure_airport, f.arrival_airport,
        (f.scheduled_arrival - scheduled_departure) duration,
        to_char(f.scheduled_departure, 'ID'::text)::integer day_of_week
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
        fdw.duration, array_agg(fdw.day_of_week) days_of_week
    FROM single_flight_per_day_of_week fdw
    GROUP BY 1, 2, 3, 4, 5
)
SELECT fw.flight_no, fw.aircraft_code,
    dep.city->>'ru' dep_city, fw.departure_airport, dep.airport_name->>'ru' dep_airpot_name,
    arr.city->>'ru' arr_city, fw.arrival_airport, arr.airport_name->>'ru' arr_airport_name,
    fw.duration, fw.days_of_week
FROM single_flight_per_week fw
    JOIN airports_data dep ON dep.airport_code = fw.departure_airport
    JOIN airports_data arr ON arr.airport_code = fw.arrival_airport;

-- RECURSIVE VIEW
CREATE OR REPLACE RECURSIVE VIEW numbers(n) AS
VALUES (1)
UNION ALL
SELECT n + 1 FROM numbers WHERE n < 10;

SELECT * FROM numbers;
