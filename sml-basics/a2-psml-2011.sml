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
