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

fun factorial5 n =
    let fun fact (0, res) = res
          | fact (n, res) = fact (n - 1, n * res)
    in
        fact (n, 1)
    end;

(* factorial5 5; *)

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
val myList : int list = 1 :: 2 :: 3 :: 4 :: nil;
val myList2 : int list = [1, 2, 3, 4];

(* Indictive, recursive function definition *)
(* Function type : 'a list -> int *)
fun myLength nil = 0
  | myLength (_ :: t) = 1 + myLength t;

(* myLength myList2; *)

(* Function type : 'a list * 'a list -> 'a list *)
fun myAppend (nil, l) = l
  | myAppend (h :: t, l) = h :: myAppend (t, l);

(* myAppend (myList, myList2); *)

fun myReverse nil = nil
  | myReverse (h :: t) = myReverse (t) @ [h];

(* myReverse myList2; *)

(* Tail-recursive reverse *)
local
    (* r accumulates result before recursive calls that consume input list *)
    fun reverseResult (nil, r) = r
      | reverseResult (h :: t, r) = reverseResult (t, h :: r)
in
fun myReverse2 l = reverseResult (l, nil)
end;

(* myReverse2 myList2; *)

fun cons (x, l) = x :: l;

(* cons (1, nil); *)

(* datatype: new type constructos and value constructors *)
datatype suit = Spades | Hearts | Diamonds | Clubs;

(* parametrized datatype *)
datatype 'a myoption = None | Some of 'a;

(* Recursive function with myoption *)
fun myexpt (None, n) = myexpt (Some 2, n)
  | myexpt (Some b, 0) = 1
  | myexpt (Some b, n) =
    if n mod 2 = 0
    then myexpt (Some (b * b), n div 2)
    else b * myexpt (Some b, n - 1);

(* myexpt (None, 3); *)
(* myexpt (Some 3, 4); *)

fun myReciprocal 0 = None
  | myReciprocal x = Some (1 div x);

(* myReciprocal 0; *)
(* myReciprocal 2; *)

(* Tree: recursive datatype *)
datatype 'a mytree =
         Empty | Node of 'a mytree * 'a * 'a mytree;

(* Function on recursive datatype *)
fun myHeight Empty = 0
  | myHeight (Node (left, _, right)) =
    (* Multiple recursion *)
    1 + Int.max (myHeight left, myHeight right);

fun mySize Empty = 0
  | mySize (Node (left, _, right)) =
    1 + mySize left + mySize right;

(* Forest: mutually recursive datatype *)
datatype 'a mytree2 =
         Empty2 | Node2 of 'a * 'a myforest
     and 'a myforest =
         None2 | Tree2 of 'a mytree2 * 'a myforest;

(* Function on mutually recursive datatype *)
fun myTreeSize Empty2 = 0
  | myTreeSize (Node2 (_, f)) = 1 + myForestSize f
and myForestSize None2 = 0
  | myForestSize (Tree2 (t, f2)) = myTreeSize t + myForestSize f2;

datatype intOrString = Int of int | String of string;

(* let *)
(*     val number = Int 1 *)
(*     val text = String "a" *)
(* in *)
(*     case number of *)
(*         Int n => "Number: " ^ Int.toString n *)
(*       | String s => "Text: " ^ s *)
(* end *)

(* datatype declarations and pattern matching *)
(* for definging abstract syntax of a lenguage *)
(* datatype defines an arithmetic langauge with numbers and operations *)
datatype expr =
         Numeral of int |
         Plus of expr * expr |
         Times of expr * expr;

(* Pattern matching allows for easy evaluation of the arithmetic langauge expressions *)
fun eval (Numeral n) = Numeral n
  | eval (Plus (e1, e2)) =
    let
        val Numeral n1 = eval e1
        val Numeral n2 = eval e2
    in
        Numeral (n1 + n2)
    end
  | eval (Times (e1, e2)) =
    let
        val Numeral n1 = eval e1
        val Numeral n2 = eval e2
    in
        Numeral (n1 * n2)
    end;

(* let *)
(*     val e = Plus (Numeral 1, Times (Numeral 2, Numeral 3)) *)
(* in *)
(*     eval e *)
(* end; *)

(* Higher-order functions *)
(* Non-tail-recursive map *)
fun myMap (f, nil) = nil
  | myMap (f, h :: t) = (f h) :: myMap (f, t);

(* myMap (fn x => x * 10, [1, 2, 3, 4, 5]); *)

(* Tail-recursive map *)
local
    fun mapResult (f, nil, res) = res
      | mapResult (f, h :: t, res) = mapResult (f, t, res @ [f h])
in
fun myMap2 (f, l) = mapResult (f, l, nil)
end;

(* myMap2 (fn x => x * 10, [1, 2, 3, 4, 5]); *)

(* Function type: 'a -> ('b -> 'a) *)
val constantly = fn k => (fn a => k);

(* constantly2 is equivalent to constantly above. Note SPACE between arguemnts *)
fun constantly2 k a = k;

(* let *)
(*     val c1 = constantly2 1 *)
(*     val c2 = constantly2 "Vlad" *)
(* in *)
(*     (c1 (), c2 ()) *)
(* end; *)

(* Curried map: ('a -> 'b) -> 'a list -> b' list *)
fun myMap3 f nil = nil
  | myMap3 f (h :: t) = (f h) :: (myMap3 f t);

(* myMap3 (fn x => x * 10) [1, 2, 3, 4, 5]; *)

(* curry type: ('a * b' -> 'c) -> ('a -> ('b -> c')) *)
fun myCurry f x y = f (x, y);

(* Curry two-argument myMap2 to get curried myMap4 *)
fun myMap4 f l = myCurry myMap2 f l;

(* myMap4 (fn x => x * 10) [1, 2, 3, 4, 5]; *)

(* Functions abstract patterns of control: reduce *)
fun myReduce (unit, opn, nil) = unit
  | myReduce (unit, opn, h :: t) = opn (h, myReduce (unit, opn, t));

(* myReduce (0, op +, []); *)
(* myReduce (0, op +, [1, 2, 3, 4, 5]); *)

(* Tail-recursive reduce *)
local
    fun reduceResult (unit, opn, nil, res) = res
      | reduceResult (unit, opn, h :: t, res) =
        reduceResult (unit, opn, t, (opn (h, res)))
in
fun myReduce2 (unit, opn, l) = reduceResult (unit, opn, l, unit)
end;

myReduce2 (0, op +, []);
myReduce2 (0, op +, [1, 2, 3, 4, 5]);

fun addUp l = myReduce2 (0, op +, l);

(* addUp [1, 2, 3, 4, 5]; *)

fun mulUp l = myReduce2 (1, op *, l);

(* mulUp [1, 2, 3, 4, 5]; *)

fun consUp l = myReduce2 (nil, op ::, l);

(* consUp [1, 2, 3, 4, 5]; *)

(* Functions allow for staging computation: curry *)
fun makeReduce (unit, opn) =
    let
        fun rdc nil = unit
          | rdc (h :: t) = opn (h, rdc t)
    in
        (* Return function of list with unit and operation already defined *)
        rdc
    end;

val mySum = makeReduce (0, op +);

(* mySum [1, 2, 3, 4, 5]; *)

val myProd = makeReduce (1, op *);

(* myProd [1, 2, 3, 4, 5]; *)

(* Curried makeReduce *)
fun makeReduce2 unit opn l = myReduce (unit, opn, l);

(* Early arguments *)
val mySum2 = makeReduce2 0 (op +);

(* Late argumet *)
(* mySum [1, 2, 3, 4, 5]; *)

val myProd2 = makeReduce2 1 (op *);

(* myProd2 [1, 2, 3, 4, 5]; *)

fun myAppend (nil, l) = l
  | myAppend (h :: t, l) = h :: (myAppend (t, l));

myAppend ([1, 2, 3], [4, 5, 6]);

(* Curried append *)
fun myAppend2 l s = myAppend (l, s);

val myAppend2l = myAppend2 [1, 2, 3];

(* myAppend2l [4, 5, 6]; *)

(* Recursive map *)
(* myMap5 type: ('a -> b') * 'a list -> b' list *)
fun myMap5 (f, nil) = nil
  | myMap5 (f, h :: t) = (f h) :: myMap5 (f, t);

(* myMap5 ((fn x => x * 10), [1, 2, 3, 4, 5]); *)

(* Curried map *)
(* myMap6 type: ('a -> b') -> 'a list -> b' list *)
fun myMap6 f l = myMap5 (f, l);

(* val myMap6f = myMap6 (fn x => x * 10); *)
(* myMap6f [1, 2, 3, 4, 5]; *)
(* myMap6 (fn x => x * 10) [1, 2, 3, 4, 5]; *)

(* Exceptions *)

(* Div: divide by zero *)
(* 3 div 0;  *)

(* Match: Inexhaustive match *)
(* fun myHead (h:: _) = h; *)
(* myHead []; *)

(* Bind: inexhaustive binding *)
(* val h::_ = nil; *)

(* User-defined exceptions *)
exception Factorial;

fun factorial6 n =
    (* Inefficiency: heck for negatives on every recursive call *)
    if n < 0 then
        raise Factorial
    else if n = 0 then
        1
    else
        n * factorial6 (n - 1);

(* factorial6 5; *)
(* factorial6 ~5; *)

local
    fun fact 0 = 1
      | fact n = n * fact (n - 1)
in
fun factorial7 n =
    if n < 0 then
        raise Factorial
    else
        fact n
end;

(* factorial7 5; *)
(* factorial7 ~5; *)

(* Non-local transfer of control *)
fun computeFactorial () =
    (* Expression that may thorw an exception *)
    let
        val nStr = valOf (TextIO.inputLine TextIO.stdIn)
        val n = valOf (Int.fromString nStr)
        val fact = factorial7 n
        val factStr = Int.toString fact
        val _ = print (factStr ^ "\n")
    in
        (* Factorial REPL *)
        computeFactorial ()
    end
    (* Set of pattern matched exception handlers *)
    handle Factorial =>
           let
               val _ = print ("Factorial: negative argument\n")
           in
               (* Restart computation on Factorial error *)
               computeFactorial ()
           end
         (* Finish computation with an error *)
         | Option => print ("Factorial: error!\n");

(* computeFactorial (); *)
