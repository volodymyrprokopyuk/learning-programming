(* Value bindings *)
val pi : real = 3.14;
val s : real = Math.sin pi;

(* Local declarations and expressions *)
let
    val m : int = 2 + 3
    val n : int = m * m
in
    m * n
end;

(* Temporal variable shadowing *)
val m : int = 1;
val n : int =
    let
        (* Outer m is temporarily shadowed *)
        val m : int = 10
    in
        m
    end;
