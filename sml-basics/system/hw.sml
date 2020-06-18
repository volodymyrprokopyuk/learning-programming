structure HelloWorld =
struct
fun main (program, arguments) =
    let
        val _ = print ("Program: " ^ program ^ "\n")
        val args = String.concatWith " " arguments
        val _ = print ("Arguments: " ^ args ^ "\n")
    in
        OS.Process.success
    end
end
