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
