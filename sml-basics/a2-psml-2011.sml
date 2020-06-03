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
val (x1, q1) = Q.remove q;
(* open structure *)
open Queue;
val (x2, q2) = remove q1;
val (x3, q3) = remove q2;
val (x4, q4) = remove q3;
