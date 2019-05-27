module Main

main : IO ()
-- main = putStrLn "Hello Idris"
-- main = putStrLn ?greeting
-- main = putStrLn (?convert 'x')
main = putStrLn (cast 'x')

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
