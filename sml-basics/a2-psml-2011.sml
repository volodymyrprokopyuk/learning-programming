(* Chapters 18- *)

(* Signature specification *)
signature QUEUE =
sig
    (* Unary type constructor *)
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
    val isEmpty : 'a queue -> bool
end;

(* Signature specialization *)
signature QUEUE_AS_LIST =
QUEUE where type 'a queue = 'a list * 'a list;

(* Structure declaration *)
structure Queue =
struct
type 'a queue = 'a list * 'a list
exception Empty
val empty = (nil, nil)
fun insert (x, (b, f)) = (x :: b, f)
fun remove (nil, nil) = raise Empty
  | remove (bs, nil) = remove (nil, rev bs)
  | remove (bs, f :: fs) = (f, (bs, fs))
end;
