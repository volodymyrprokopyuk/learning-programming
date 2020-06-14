(* Chapters 18- *)

(* Signature specification *)
signature QUEUE =
sig
    (* Unary type constructor: abstract type *)
    type 'a queue
    (* Nullary exception constructor *)
    exception Empty
    (* Polymorphic values *)
    val empty : 'a queue
    (* Polymorphic functions *)
    val insert : 'a * 'a queue -> 'a queue
    val remove : 'a queue -> 'a * 'a queue
end;

(* Signature inclusion  *)
signature QUEUE_WITH_EMPTY =
sig
    include QUEUE
    (* Add new operation: new value component *)
    val isEmpty : 'a queue -> bool
end;

(* Signature specialization *)
signature QUEUE_AS_LIST =
(* Specialize 'a queue abstract type with more concrete type *)
QUEUE where type 'a queue = 'a list * 'a list;

(* Structure declaration *)
(* Opaque/restrictive signature ascription to enfoce data abstraction *)
(* QUEUE_AS_LIST has specialized 'a queue abstract type *)
(* so the insert function can be used *)
structure Queue :> QUEUE_AS_LIST =
(* structure Queue : QUEUE = *)
struct
type 'a queue = 'a list * 'a list
exception Empty
val empty = (nil, nil)
fun insert (x, (b, f)) = (x :: b, f)
fun remove (nil, nil) = raise Empty
  | remove (bs, nil) = remove (nil, rev bs)
  | remove (bs, f :: fs) = (f, (bs, fs))
end;

(* Use Queue *)
val q = Queue.insert (10, ([6, 5, 4], [1, 2, 3]));
(* Structure abbreviation *)
structure Q = Queue;
(* val (x1, q1) = Q.remove q; *)
(* open structure *)
open Queue;
(* val (x2, q2) = remove q1; *)
(* val (x3, q3) = remove q2; *)
(* val (x4, q4) = remove q3; *)

signature VALUE =
sig
    type a
    val value : a
end;

(* Opaque signature ascription: type hiding *)
(* structure Value :> VALUE = *)
(* Transparent signature ascription: type exposure *)
structure Value : VALUE =
struct
type a = int
val value = 4
end;

(* val v = Value.value; *)

(* string key dictionary *)
signature STR_DICT =
sig
    type 'a dict
    val empty : 'a dict
    val insert : 'a dict * string * 'a -> 'a dict
    val lookup : 'a dict * string -> 'a option
end;

structure StrDict :> STR_DICT =
struct
datatype 'a dict = Empty | Node of 'a dict * string * 'a * 'a dict
val empty = Empty
fun insert (Empty, k, v) = Node (Empty, k, v, Empty)
fun lookup (Empty, _) = NONE |
    lookup (Node (dl, l, v, dr), k) =
    if k < l then lookup (dl, k)
    else if k > l then lookup (dr, k)
    else SOME v
end;

(* let *)
(*     open StrDict *)
(*     val sd = insert (empty, "a", 1) *)
(* in *)
(*     lookup (sd, "a") *)
(* end; *)

(* Generic key dictionary *)
signature GEN_DICT =
sig
    (* Abstract type of dictionary key *)
    type key
    type 'a dict
    val empty : 'a dict
    val insert : 'a dict * key * 'a -> 'a dict
    val lookup : 'a dict * key -> 'a option
end;

(* Signature specialization *)
signature INT_DICT =
GEN_DICT where type key = int;

structure IntDict :> INT_DICT =
struct
type key = int
datatype 'a dict = Empty | Node of 'a dict * key * 'a * 'a dict
val empty = Empty
fun insert (Empty, k, v) = Node (Empty, k, v, Empty)
fun lookup (Empty, _) = NONE
  | lookup (Node (dl, l, v, dr), k) =
    if k < l then lookup (dl, k)
    else if k > l then lookup (dr, k)
    else SOME v
end;

(* let *)
(*     open IntDict *)
(*     val id = insert (empty, 1, "a") *)
(* in *)
(*     lookup (id, 1) *)
(* end; *)

(* Ordered key signature *)
signature ORDERED =
sig
    type t
    val eq : t * t -> bool
    val lt : t * t -> bool
end;

(* Lexicographical string ordering *)
structure LexStr : ORDERED =
struct
type t = string
fun eq (a : string, b : string) = a = b
fun lt (a : string, b : string) = a < b
end;

(* Ascending integer ordering *)
structure AscInt : ORDERED =
struct
type t = int
val eq = (op =)
val lt = (op <)
end;

signature DICT =
sig
    (* Substructure abstracts key type and key ordering *)
    structure Key : ORDERED
    type 'a dict
    val empty : 'a dict
    val insert : 'a dict * Key.t * 'a -> 'a dict
    val lookup : 'a dict * Key.t -> 'a option
end;

signature LEX_STR_DICT =
DICT where type Key.t = string;

signature ASC_INT_DICT =
DICT where type Key.t = int;

structure LexStrDict :> LEX_STR_DICT =
struct
structure Key = LexStr
datatype 'a dict = Empty | Node of 'a dict * Key.t * 'a * 'a dict
val empty = Empty
fun insert (Empty, k, v) = Node (Empty, k, v, Empty)
fun lookup (Empty, _) = NONE
  | lookup (Node (dl, l, v, dr), k) =
    if Key.lt(k, l) then lookup (dl, k)
    else if Key.lt(l, k) then lookup (dr, k)
    else SOME v
end;

(* let *)
(*     open LexStrDict *)
(*     val lsd = insert (empty, "a", 1) *)
(* in *)
(*     lookup (lsd, "a") *)
(* end; *)

(* Geometry in ML: space dimension is not part of the specification *)
signature VECTOR =
sig
    type vector
    (* Unit element for addition *)
    val zero : vector
    val scale : real * vector -> vector
    val add : vector * vector -> vector
    val dot : vector * vector -> real
end;

signature POINT =
sig
    structure Vector : VECTOR
    type point
    (* Move a point along a vector *)
    val translate : point * Vector.vector -> point
    (* Create a vector as a difference between two points *)
    val ray : point * point -> Vector.vector
end;

signature SPHERE =
sig
    structure Vector : VECTOR
    structure Point : POINT
    sharing Point.Vector = Vector
    type sphere
    (* Create sphere from a center and a radius *)
    val sphere : Point.point * Vector.vector -> sphere
end;

signature GEOMETRY =
sig
    structure Point : POINT
    structure Sphere : SPHERE
    sharing Sphere.Point = Point
            and Sphere.Vector = Point.Vector
end;

(* Structures, and not signatures, specify the dimension *)
(* structure Geometry2D :> GEOMETRY = ... *)
(* structure Geometry3D :> GEOMETRY = ... *)

(* Functor: generic/parametrized module *)
(* Functor: takes a structure as argument, yeilds a structure as argument *)
(* Functor: single implementation parameterized with argument structure *)
(* Function binding *)
functor DictFunctor (structure K : ORDERED) :>
        DICT where type Key.t = K.t =
struct
structure Key = K
datatype 'a dict = Empty | Node of 'a dict * Key.t * 'a * 'a dict
val empty = Empty
fun insert (Empty, k, v) = Node (Empty, k, v, Empty)
fun lookup (Empty, _) = NONE
  | lookup (Node (dl, l, v, dr), k) =
    if Key.lt(k, l) then lookup (dl, k)
    else if Key.lt(l, k) then lookup (dr, k)
    else SOME v
end;

(* Function application *)
structure LexStrDictF = DictFunctor (structure K = LexStr);

(* let *)
(*     open LexStrDictF *)
(*     val lsd = insert (empty, "a", 1) *)
(* in *)
(*     lookup (lsd, "a") *)
(* end; *)

structure AscIntDictF = DictFunctor (structure K = AscInt);

(* let *)
(*     open AscIntDictF *)
(*     val aid = insert (empty, 1, "a") *)
(* in *)
(*     lookup (aid, 1) *)
(* end; *)

(* 2D and 3D GEOMETRY with functors *)

(* functor PointFunctor (structure V : VECTOR) : POINT = *)
(* struct *)
(* structure Vector = V *)
(* end; *)

(* functor SphereFunctor (structure V : VECTOR structure P : POINT) : SPHERE = *)
(* struct *)
(* structure Vector = V *)
(* structure Point = P *)
(* end; *)

(* functor GeometryFunction (structure P : POINT structure S : SPHERE) : GEOMETRY = *)
(* struct *)
(* structure Point = P *)
(* structure Sphere = S *)
(* end; *)

(* structure Vector2D = ... *)
(* structure Point2D = PointFunctor (structure V = Vector2D); *)
(* structure Sphere2D = SphereFunctor (structure V = Vector2D structure P = Point2D); *)
(* structure Geometry2D = GeometryFunctor (structure P = Point2D structure S = Sphere2D); *)

fun factorial n = if n = 0 then 1 else n * factorial (n - 1);

(* factorial 5; *)

(* fun = val rec *)
val rec factorial2 = fn n => if n = 0 then 1 else n * factorial2 (n - 1);

(* factorial2 5; *)

(* Clausal function declaration with pattern matching *)
fun factorial3 0 = 1
  | factorial3 n = n * factorial3 (n - 1);

(* factorial3 5; *)

(* Head and tail or pattern matching*)
(* let *)
(*     val l = [1, 2, 3, 4] *)
(*     val h = hd l *)
(*     val t = tl l *)
(*     val h2 :: t2 = l *)
(* in *)
(*     (h, t, h2, t2) *)
(* end; *)

(* Curried, recursive list append *)
fun append2 nil l = l
  | append2 (h :: t) l = h :: (append2 t l);

(* let *)
(*     val l = append2 [1, 2] [3, 4, 5]; *)
(*     val a12 = append2 [1, 2] *)
(*     val l2 = a12 [3, 4, 5] *)
(* in *)
(*     (l, l2) *)
(* end; *)

(* Polymorphic function *)
fun makeList x = [x];

(* o function composition *)
fun makeListOfLists x = (makeList o makeList) x;

(* makeListOfLists 1; *)

fun map2 f nil = nil
  | map2 f (h :: t) = (f h) :: (map2 f t);

(* let *)
(*     val add1 = map2 (fn x => x + 1) *)
(* in *)
(*     add1 [1, 2, 3, 4] *)
(* end; *)

(* Binary tree: recursive data type *)
datatype 'a btree = Leaf of 'a | Node of 'a btree * 'a btree;

(* f accepts a pair *)
fun addLeaves (Leaf x) = x
  | addLeaves (Node (l, r)) = (addLeaves l) + (addLeaves r);

fun tmap f (Leaf x) = x
  | tmap f (Node (l, r)) = f ((tmap f l), (tmap f r));

(* Curried f *)
fun tmap2 f (Leaf x) = x
  | tmap2 f (Node (l, r)) = f (tmap2 f l) (tmap2 f r);

(* let *)
(*     val t = Node (Node (Leaf 1, Leaf 4), Leaf 7) *)
(*     val sumOfLeaves = addLeaves t *)
(*     (* f accepts a pair *) *)
(*     val addLeaves2 = tmap (op +) *)
(*     val sumOfLeaves2 = addLeaves2 t *)
(*     (* Curried f *) *)
(*     val addLeaves3 = tmap2 (fn x => fn y => x + y) *)
(*     val sumOfLeaves3 = addLeaves3 t *)
(* in *)
(*     (sumOfLeaves, sumOfLeaves2, sumOfLeaves3) *)
(* end; *)

(* Exceptions *)
exception Factorial of string * int;

(* Local helper function declaration *)
local
    fun f (0, r) = r
      | f (n, r) = f (n - 1, n * r)
in
fun factorial4 n =
    if n < 0 then raise Factorial ("Negative number", n)
    else f (n, 1)
end;

(* factorial4 5; *)
(* factorial4 ~5 handle Factorial (m, n) => raise Factorial (m, n); *)

(* Point-free function definiiton via currying *)
val sum = foldl (op +) 0;

(* sum [1, 2, 3, 4, 5]; *)

(* Explicit function type *)
fun incInt (x : int) : int = x + 1;

(* incInt 10; *)

(* Tuple pattern matching *)
fun myTuple (a, b) = {x = a, y = b};

(* myTuple (1, "a"); *)

(* Record pattern matching *)
fun myRecord {first = a, second = b} = {x = a, y = b};

(* myRecord {first = 1, second = "a"}; *)

(* Function composition *)
val incFactorial = incInt o factorial4;

(* incFactorial 5; *)

(* User-defined operators *)
fun ** (x, 0) = 1
  | ** (x, y) = x * ** (x, y - 1);

(* Function application *)
(* ** (2, 5); *)

(* Operator declaration: left associative, otherwise use infixr *)
infix 7 **;

(* Operator application *)
(* 2 ** 5; *)

(* Use operator as function *)
(* (op +) (2, 3); *)
(* (op ** ) (2, 5); *)

(* Abstract data types with opaque internal data representation: - *)
(* Abstract data types exposes only interface for  *)
(*     - construction: emptys *)
(*     - manipulation: isEmptys, pushs, tops, pops *)
(*     - conversion: toList *)
abstype 'a stack = Stack of 'a list
with
val emptys = Stack nil
fun isEmptys (Stack l) = l = nil
fun pushs (x, Stack l) = Stack (x :: l)
fun tops (s as Stack (x :: l)) = (x, s)
fun pops (Stack (x :: l)) = (x, Stack l)
fun toList (Stack l) = l
end;

(* let *)
(*     val s = emptys *)
(*     val s2 = pushs (1, s) *)
(*     val s3 = pushs(2, s2) *)
(*     val s4 = pushs(3, s3) *)
(*     val (x5, s5) = pops s4 *)
(* in *)
(*     (* isEmptys s *) *)
(*     (* tops s2 *) *)
(*     (* tops s3 *) *)
(*     (* toList s4 *) *)
(*     (x5, toList s5) *)
(* end; *)

(* Imperative programming *)
(* Reference variable declaration (ref), assignment (:=), and dereferencing (!) *)
(* let *)
(*     val refVar = ref 1 *)
(*     val v1 = !refVar *)
(*     val _ = refVar := 2 *)
(*     val v2 = !refVar *)
(*     val refEqSameRef = refVar = refVar *)
(*     val refVar2 = ref 2 *)
(*     val refEqDiffRef = refVar = refVar2 *)
(*     val refEqSameVal = !refVar = !refVar2 *)
(* in *)
(*     (v1, v2, refEqSameRef, refEqDiffRef, refEqSameVal) *)
(* end; *)

(* Exception, raise, handle *)
exception DivizionByZero of string;

fun // (a, b) =
    if b = 0 then raise DivizionByZero "Divisiont by zero"
    else a div b;

infix 7 //;

(* val v = 5 // 0 handle DivizionByZero m => raise DivizionByZero m; *)

(* Lexical scopeing, environemnt, and free variables in a closure *)
(* let *)
(*     val pi = 3.1415 *)
(*     (* pi is a free variable and is recorded in the evfironment for the area closure *) *)
(*     fun area r = pi * r * r *)
(*     val a = area 1.0 *)
(*     (* Rebound pi does not take effect on the resual of the area function *) *)
(*     val pi = "PI" *)
(*     val a2 = area 1.0 *)
(* in *)
(*     (a, a2) *)
(* end; *)

(* Multiple arguments: tuple and curried function *)
fun pow (m, n) = if n = 0 then 1 else m * (pow (m, n - 1));

pow (2, 5);

(* Multiple functions: curried function with partial application *)
fun power m n = if n = 0 then 1 else m * (power m (n - 1));

(* power 2 5; *)

(* Curried function allows to define new functions via partial application *)
val powerOf2 = power 2;
(* powerOf2 5; *)

(* Factorial as function composition *)
fun range n = List.tabulate (n, fn x => x + 1);

val product = foldl (op *) 1;

val factorial5 = product o range;

(* factorial5 5; *)

(* Mutual recursion *)
fun take nil = nil
  | take (h :: t) = h :: (skip t)
and skip nil = nil
  | skip (h :: t) = take t;

(* take [1, 2, 3, 4, 5, 6]; *)
(* skip [1, 2, 3, 4, 5, 6]; *)

(* Quick sort *)
local
    fun partition (x, nil) = (nil, nil)
      | partition (x, h :: t) =
        let
            val (ss, bs) = partition (x, t)
        in
            (* Integer comparison *)
            if h < x then (h :: ss, bs)
            else (ss, h :: bs)
        end
in
fun qsort nil = nil
  | qsort [x] = [x]
  | qsort (h :: t) =
    let
        val (ss, bs) = partition (h, t)
    in
        qsort ss @ [h] @ qsort bs
    end
end;

(* qsort [8, 2, 9, 5, 3, 1, 4, 0, 6, 7]; *)

(* Fibonacci sequence *)
local
    fun fib a b 0 = a
      | fib a b n = fib b (a + b) (n - 1)
in
fun fibonacci n = fib 1 1 n
end;

(* fibonacci 0; *)
(* fibonacci 1; *)
(* fibonacci 2; *)
(* fibonacci 3; *)
(* fibonacci 4; *)
(* fibonacci 5; *)
(* fibonacci 6; *)
(* fibonacci 7; *)

(* Type alias *)
type user = {id : int, name : string};

(* val u : user = {id = 1, name = "Vlad"}; *)

type 'a apair = 'a * 'a;

(* val p : char apair = (#"a", #"b"); *)

(* Imperative while loop *)
(* let *)
(*     (* Reference variable *) *)
(*     val c = ref 0; *)
(* in *)
(*     while !c < 5 do ( *)
(*         (* Command sequencing *) *)
(*         print (Int.toString (!c) ^ "\n"); *)
(*         (* Dereferencing and assignment *) *)
(*         c := !c + 1 *)
(*     ) *)
(* end; *)

(* Tuple argument *)
fun max2 (a, b) = if a > b then a else b;

val max3 = fn (a, b) => if a > b then a else b;

(* max2 (1, 2); *)
(* max3 (1, 2); *)

fun max4 a b = if a > b then a else b;

val max5 = fn a => fn b => if a > b then a else b;

(* max4 1 2; *)
(* max5 1 2; *)

(* Pattern alias *)
(* let *)
(*     val v as (a, b) = (1, "a") *)
(* in *)
(*     (v, a, b) *)
(* end; *)

(* let *)
(*     fun occur p = length o (List.filter p) *)
(*     val l = [1, 0, 4, 0, 3, 4, 0] *)
(* in *)
(*     occur (fn x => x = 0) l *)
(* end; *)

signature NAT =
sig
    type nat
    val zero : nat
    val succ : nat -> nat
end;

structure Nat :> NAT =
struct
type nat = int
val zero = 0
fun succ x = x + 1
end;

(* let *)
(*     open Nat *)
(*     val x = zero *)
(*     val y = succ x *)
(* in *)
(*     (x, y) *)
(* end; *)

(* Dictionary signature *)
signature DICTIONARY =
sig
    type (''a, 'b) dict
    val empty : (''a, 'b) dict
    val insert : ''a -> 'b -> (''a, 'b) dict -> (''a, 'b) dict
    val lookup : ''a -> (''a, 'b) dict -> 'b option
end;

(* Dictionary implementation via associaltion list *)
structure Dictionary :> DICTIONARY =
struct
type (''a, 'b) dict = (''a * 'b) list
val empty = nil
fun insert k v d = (k, v) :: d
fun lookup k nil = NONE
  | lookup k ((k', v) :: d) = if k' = k then SOME v else lookup k d
end;

(* let *)
(*     open Dictionary *)
(*     val d = insert "a" 1 empty *)
(*     val l1 = lookup "a" d *)
(*     val l2 = lookup "b" d *)
(* in *)
(*     (l1, l2) *)
(* end; *)

signature STACK =
sig
    type 'a stack
    exception EmptyStack
    val empty : 'a stack
    val push : 'a -> 'a stack -> 'a stack
    val pop : 'a stack -> 'a * 'a stack
end;

structure Stack2 :> STACK =
struct
type 'a stack = 'a list
exception EmptyStack
val empty = nil
fun push x s = x :: s
fun pop nil = raise EmptyStack
  | pop (x :: s) = (x, s)
end;

datatype postfix_atom = Int of int | Plus | Times;

functor PostfixEvalFn (S: STACK) =
struct
fun eval exp =
    let
        fun eval' (Int x, s) = S.push x s
          | eval' (Plus, s) =
            let
                val (b, s) = S.pop s
                val (a, s) = S.pop s
            in
                S.push (a + b) s
            end
          | eval' (Times, s) =
            let
                val (b, s) = S.pop s
                val (a, s) = S.pop s
            in
                S.push (a * b) s
            end
        val (res, _) = S.pop (foldl eval' S.empty exp)
    in
        res
    end
end;

structure PostfixEval = PostfixEvalFn(Stack2);

(* let *)
(*     open PostfixEval *)
(*     val exp = [Int 3, Int 4, Plus, Int 2, Times] *)
(* in *)
(*     eval exp *)
(* end; *)

(* Curried function *)
(* fun add a b = a + b; *)
(* val add1 = add 1; *)
(* val x = add1 1; *)

(* Unduplicate *)
local
    fun undup' nil r = r
      | undup' (h :: t) nil = undup' t [h]
      | undup' (h :: t) (r as (h' :: _)) =
        if h = h' then undup' t r else undup' t (h :: r)
in
fun undup l = rev (undup' l nil)
end;

(* undup [0, 0, 1, 1, 1, 0, 0, 0, 1]; *)

(* Reverse  *)
local
    fun rev' nil r = r
      | rev' (h :: t) r = rev' t (h :: r)
in
fun myRev l = rev' l nil
end;

(* myRev [1, 2, 3, 4, 5, 6, 7, 8]; *)

(* Length *)
local
    fun length' nil r = r
      | length' (h :: t) r = length' t (r + 1)
in
fun myLength l = length' l 0
end;

(* myLength [1, 2, 3, 4]; *)

fun subList l (j, k) =
    let
        fun subList' nil _ r = r
          | subList' (h :: t) i r =
            if i > k then r
            else if i >= j andalso i < k
            then subList' t (i + 1) (h :: r)
            else subList' t (i + 1) r
    in
        if j >= k orelse j > (length l) orelse j < 0
        then nil
        else rev (subList' l 0 nil)
    end;

(* subList nil (0, 1); *)
(* subList [0] (3, 2); *)
(* subList [0] (~3, ~2); *)
(* subList [0] (0, 2); *)
(* subList [0, 1] (0, 1); *)
(* subList [0, 1, 2, 3, 4, 5] (2, 4); *)

local
    fun member x l = List.exists (fn x' => x' = x) l
    fun unique' nil r = r
      | unique' (h :: t) r =
        (* Get last unique element *)
        (* if member h t *)
        (* Get first unique element *)
        if member h r
        then unique' t r
        else unique' t (h :: r)
in
fun unique l = rev (unique' l nil)
end;

(* unique [1, 2, 1, 3, 2, 4, 5, 1]; *)

(* Interleave, concat, permute *)
fun interleave x nil = [[x]]
  | interleave x (h :: t) =
    (x :: h :: t) :: (map (fn l => h :: l) (interleave x t));

(* interleave 10 [1, 2, 3]; *)

fun concat nil = nil
  | concat (h :: t) = h @ (concat t);

(* concat [[1], [2], [3]]; *)

fun permute nil = [nil]
  | permute (h :: t) = concat (map (interleave h) (permute t));

(* permute [1, 2, 3]; *)

local
    fun isPalindrome' nil nil = true
      | isPalindrome' (h :: t) (h' :: t') = h = h' andalso isPalindrome' t t'
in
fun isPalindrome l = isPalindrome' l (rev l)
end;

(* isPalindrome [1, 2, 3, 3, 2]; *)
(* isPalindrome [1, 2, 3, 3, 2, 1]; *)

(* Fold right *)
fun foldr f b nil = b
  | foldr f b (h :: t) = f (h, (foldr f b t));

(* foldr (op +) 0 [1, 2, 3, 4, 5]; *)
(* foldr (op -) 0 [5, 4, 7]; *)

(* Fold left: new base *)
fun foldl f b nil = b
  | foldl f b (h :: t) = foldl f (f (h, b)) t;

(* foldl (op +) 0 [1, 2, 3, 4, 5]; *)
(* foldl (op -) 0 [5, 4, 7]; *)
