WITH RECURSIVE t(n) AS ( -- relation type definition
    VALUES (1) -- initial tuple
    UNION ALL -- recursively append new tuple to the finaly relation
    -- recursive step
    SELECT n + 1 FROM t WHERE n < 10 -- stop condition
)
SELECT * FROM t; -- execute recursive query
