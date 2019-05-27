module Main

-- * Standard types *
--     - Numerics
--         - Int fixed-length, signed
--         - Integer unbounded, signed
--         - Nat unbounded, unsigned, not negative
--         - Double double-precision float
--     - Strings
--         - Char 'a'
--         - String "ab"
--     - Booleans
--         - Bool: True | False
--         - if x /= y then "Not equial" else "Equal"
-- * Funcitons *
--     - Function type declaration (required): f : a -> b
--     - Funciton body definition with equitions: f x = y
--     - An expression is evaluated by rewriting/reducing the expression according to
--       the function equations until to further rewriting could be done

-- Optional argument name (str)
average : (str : String) -> Double
average str =
    -- Local varaible definitions
    let
        numWords = wordCount str
        totalLength = sum (allLength (words str))
    in
        -- Final function result
        cast totalLength / cast numWords
    -- Local funciton definitions
    where
        wordCount : String -> Nat
        wordCount str = length (words str)

        allLength : List String -> List Nat
        allLength strs = map length strs

showAverage : String -> String
showAverage str = "The average word length is: " ++ show (average str) ++ "\n"

-- Main.main is always entry point to an Idris program
main : IO ()
-- Read from STDIN, Evaluate a function, Print result to STDOUT, Loop
main = repl "Enter a stirng: " showAverage

--------------------------------------------------------------------

-- Function is a set of equations that directs expression evaluation by
-- rewriting/reduction
doubleInt : (x : Int) -> Int
doubleInt x = x + x

-- Function partial application
add : (x : Int) -> (y : Int) -> Int
add x y = x + y

addOne : (y : Int) -> Int
addOne = add 1

-- Type variables
identity : (x : t) -> t
identity x = x

-- Dependent types in action
--     - THE VALUE (t) OF AN EARLIER ARGUMENT GIVES THE TYPE OF A LATER ARGUMENT (val)
--     - Usage> theType Double (cast 1)
theType : (t : Type) -> (val : t) -> t
theType t val = val

-- Type constraint (Num)
double : Num t => t -> t
double x = x + x

-- Higher-order function
twice : (t -> t) -> t -> t
twice f x = f (f x)

quadruple : Num t => t -> t
quadruple = twice double

-- Higher-order function with partial application of an operator
addFour : Num t => t -> t
addFour = twice (2 +)

-- Higher-order function with anonymous function
powFour : Num t => t -> t
powFour = twice (\x => x * x) -- anonymous function

-- Function local definitions
longerString : String -> String -> Nat
longerString s1 s2 =
    -- let bindings defines funciton local variables
    -- Use let to break complex expressions
    let
        l1 = length s1
        l2 = length s2
    in
        if l1 >= l2 then l1 else l2

-- Function local definitions
pythagoras : Double -> Double -> Double
pythagoras x y = sqrt (square x + square y)
  -- where defines function local functions
  -- Use where to break large functions in local scope
  where
      square : Double -> Double
      square x = x * x

myAverage : String -> Double
myAverage str =
    let
        listOfWords = words str
        numberOfWords = length listOfWords
        totalLength = the Double $ cast $ sum $ map length listOfWords
    in
        totalLength / cast numberOfWords
