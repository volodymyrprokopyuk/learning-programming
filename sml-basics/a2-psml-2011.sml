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

factorial4 5;
factorial4 ~5 handle Factorial (m, n) => raise Factorial (m, n);
