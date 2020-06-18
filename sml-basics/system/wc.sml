structure Wc =
struct
(* State machine to count words *)
local
    fun outState nil count = count
      | outState (h :: t) count =
        if Char.isSpace h
        then outState t count
        else inState t count
    and inState nil count = count + 1
      | inState (h :: t) count =
        if Char.isSpace h
        then outState t (count + 1)
        else inState t count
in
fun wordCount s = outState (explode s) 0
end

fun wordCount2 s = length (String.tokens Char.isSpace s)

fun main (prog, args) =
    let
        val text = String.concatWith " " (tl args)
        val wc = wordCount text
        val _ = print ("Word count: " ^ (Int.toString wc))
    in
        OS.Process.success
    end
end
