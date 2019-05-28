module Main

{-
* First-class types *
    - Treat types as values
    - Function can compute types
    - Prove program correctness
* Dependent types *
    - Types are dependent on other values
    - append : Vect m elem -> Vect n elem -> Vect (m + n) elem
    - Allow to express detailed properties of programs
    - Simple: Vect (Concrete type)
    - > Generic Vect m (m is type variable)
    - > Dependent Vect 4 m (vector length)
* Type-driven development *
    - 1. Type -> model
    - 2. Define -> functions
    - 3. Refine -> types and functions
-}

main : IO ()
-- main = putStrLn "Hello Idris"
-- main = putStrLn ?greeting
-- main = putStrLn (?convert 'x')
main = putStrLn (cast 'x')

-------------------------------------------------------------

-- Funciton that computes a type
StringOrInt : Bool -> Type
StringOrInt x =
    case x of
        True => String
        False => Int

-- Function that computes a value
getStringOrInt : (x : Bool) -> StringOrInt x
getStringOrInt x =
    case x of
       True => "Vlad"
       False => 4

-- Function that requires a value and casts it to a String
valToStirng : (x : Bool) -> StringOrInt x -> String
valToStirng x val =
    case x of
       True => val
       False => cast val
