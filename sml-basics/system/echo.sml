structure Echo =
struct
fun echo nil = ()
  | echo [h] = print h
  | echo (h :: t) = (print (h ^ " "); echo t)

fun main (_, nil) = OS.Process.success
  | main (_, h :: t) = (echo t; OS.Process.success)
end
