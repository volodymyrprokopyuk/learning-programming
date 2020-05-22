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
(* (fn x : real => Math.sqrt (Math.sqrt x)) 16.0; *)

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

(* Global, parameter, and local value shadowing *)
(* Global value binding *)
(* val x : int = 1; *)
(* Parameter value binding *)
fun shadow (x : int) : int =
    (* Local value binding *)
    let val x : int = 3 in x end + x;
(* shadow 2; *)
(* x; *)

(* Tuples: product type *)
(* val nullPair : unit = (); *)
(* val pair : int * string = (1, "Vlad"); *)
(* val triple : bool * real * char = (true, 1.0, #"A"); *)
(* val pairOfPairs : (int * int) * (real * real) = ((1, 2), (3.0, 4.0)); *)

(* Tuple pattern matching *)
(* val (_, pairSecond : string) = pair; *)
(* val ((one : int, _), (_, four : real)) = pairOfPairs; *)
(* val (a : int * int, b : real * real) = pairOfPairs; *)

(* Records: labeled tuples *)
type hyperlink = {
    protocol : string,
    address : string,
    display : string
};

(* val mailRecord : hyperlink = { *)
(*     protocol = "mailto", *)
(*     address = "vlad@gmail.com", *)
(*     display = "Vlad" *)
(* }; *)
(* val {protocol = mProt, address = mAddr, display = mDisp} = mailRecord; *)
(* Wildcard pattern *)
(* val {protocol = mPort2, address = _, display = _} = mailRecord; *)
(* Ellipsis pattern *)
(* val {protocol = mPort3, ...} = mailRecord; *)
(* Abbreviated form of record pattern *)
(* val {protocol, address, display} = mailRecord; *)

val euclideanDistance : real * real -> real =
 fn (x : real, y : real) => Math.sqrt (x * x + y * y);

(* euclideanDistance (3.0, 4.0); *)

(* Multiple positional parameters -> tuple pattern *)
fun euclideanDistance2 (x : real, y : real) : real =
    Math.sqrt (x * x + y * y);

(* euclideanDistance2 (3.0, 4.0); *)

(* Multiple keyword parameters -> record pattern *)
fun euclideanDistance3 {x = xParam : real, y = yParam : real} =
    Math.sqrt (xParam * xParam + yParam * yParam);

(* euclideanDistance3 {y = 4.0, x = 3.0}; *)

(* Function may return multiple results by yielding tuple or record*)

fun distances (x : real, y : real) : real * real =
    (Math.sqrt (x * x + y * y), y - x);

(* val (eucDist : real, linDist : real) = distances (3.0, 4.0); *)

(* Clausal function expression *)
val classifyInt : int -> string =
 fn 0 => "zero"
  | n : int => "non-zero";

(* (classifyInt 0); *)
(* (classifyInt 1); *)

(* Clausal function declaration *)
fun classifyInt2 0 = "zero"
  | classifyInt2 (n : int) = "non-zero";

(* (classifyInt2 0); *)
(* (classifyInt2 1); *)

fun classifyInt3 0 = "zero"
  (* Catch-all clause *)
  | classifyInt3 _ = "non-zero";

(* classifyInt3 0; *)
(* classifyInt3 1; *)

(* case analysis is implemetned in term of fn function expression*)
fun classifyInt4 (n : int) : string =
    case n of
        0 => "zero"
      | n : int => "non-zero";

(* (classifyInt4 0); *)
(* (classifyInt4 1); *)

fun classifyInt5 (n : int) : string =
    case n of
        0 => "zero"
      (* Catch-all clause *)
      | _ => "non-zero";

(classifyInt5 0);
(classifyInt5 1);

(* Negation function *)
fun myNot true = false
  | myNot false = true;

(* myNot true; *)
(* myNot false; *)

fun myNot2 (x : bool) : bool = if x then false else true;

(* myNot2 true; *)
(* myNot2 false; *)

(* Recursive functions *)
val rec factorial : int -> int =
 fn 0 => 1 | n : int => n * factorial (n - 1);

(* factorial 5; *)

fun factorial2 0 = 1
  | factorial2 (n : int) = n * factorial2 (n - 1);

(* factorial2 5; *)

(* Tail-recursive factorial with a helper function *)
(* n - for recursion control, res - for result tracking *)
fun factorialResult (0, res : int) = res
  (* recursion decrement and result computation happens before recursive call *)
  | factorialResult (n : int, res : int) = factorialResult (n - 1, n * res);

fun factorial3 (n : int) = factorialResult (n, 1);

(* factorial3 5; *)

(* Tail-recursive factorial with local declaration of helper function *)
local
    fun fResult (0, res : int) = res
      (* Tail-recursive call is the LAST STEP in evaluation of *)
      (* the recursive function application -> is substituted by simple interation *)
      | fResult (n : int, res : int) = fResult (n - 1, n * res)
in
fun factorial4 (n : int) = fResult (n, 1)
end;

(* factorial4 5; *)

fun fibonacci 0 = 1
  | fibonacci 1 = 1
  | fibonacci (n : int) = fibonacci (n - 1) + fibonacci (n - 2);

fibonacci 10;

fun fibonacci2 0 = (1, 0)
  | fibonacci2 1 = (1, 1)
  | fibonacci2 (n : int) =
    let
        val (current : int, previous : int) = fibonacci2 (n - 1)
    in
        (current + previous, current)
    end;

(* fibonacci2 10; *)

(* Mutually recursive functions *)
fun even 0 = true
  | even n = odd (n - 1)
and odd 0 = false
  | odd n = even (n - 1);

(* even 10; *)
(* odd 10; *)
(* even 11; *)
(* odd 11; *)

(* Type inference *)
(* (fn s => s ^ "\n") "Vlad" *)

val identity : 'a -> 'a = fn x => x;

(* identity 0; *)

fun identity2 (x : 'a) : 'a = x;

(* identity2 identity; *)

(* let *)
(*     (* val double = fn x => x + x *) *)
(*     fun double x = x + x *)
(* in *)
(*     double 1 *)
(* end; *)

(* Lists: recutsive type with inductive definition*)
(* Type constructors are parameterized types *)
(* Value cunstructors for type list: nil, :: (cons) *)
