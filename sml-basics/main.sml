(* Value bindings *)
(* val pi : real = 3.14; *)
(* val s : real = Math.sin pi; *)

(* Local declarations and expressions *)
(* let *)
(*     val m : int = 2 + 3 *)
(*     val n : int = m * m *)
(* in *)
(*     m * n *)
(* end; *)

(* Temporal variable shadowing *)
(* val m : int = 1; *)
(* val n : int = *)
(*     let *)
(*         (* Outer m is temporarily shadowed *) *)
(*         val m : int = 10 *)
(*     in *)
(*         m *)
(*     end; *)

(* Immediately applied anonymous function lambda expression *)
(* (fn x : real => Math.sqrt (Math.sqrt x)) 16.0 *);

(* Bind lambda expression to a variable *)
val forthRoot : real -> real =
    fn x : real => Math.sqrt (Math.sqrt x);

(* forthRoot 81.0; *)

(* Function definition = bind lambda expression *)
fun forthRoot2 (x : real) : real =
    Math.sqrt (Math.sqrt x);

(* forthRoot2 256.0; *)

fun square (x: real) : real =
    x * x;

(* square 5.0; *)

fun isEven (x : int) : bool =
    x mod 2 = 0;

(* isEven 3; *)
(* isEven 4; *)
