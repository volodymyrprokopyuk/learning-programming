(* Usage: [-h] [-v | --verbose] --width width --height hieght files *)
structure GetOpt =
struct
type cmdopt = string * string option
type cmdline = (cmdopt list) * (string list)
exception GetOpt of string

local
    fun parse' nil r = r
      | parse' ("-h" :: t) (os, fs) =
        parse' t (("help", NONE) :: os, fs)
      (* OR patterns *)
      | parse' ("-v" :: t) (os, fs) =
        parse' t (("verbose", NONE) :: os, fs)
      | parse' ("--verbose" :: t) (os, fs) =
        parse' t (("verbose", NONE) :: os, fs)
      | parse' ("--width" :: v :: t) (os, fs) =
        parse' t (("width", SOME v) :: os, fs)
      | parse' ("--height" :: v :: t) (os, fs) =
        parse' t (("hieght", SOME v) :: os, fs)
      | parse' (h :: t) (os, fs) =
        if String.isPrefix "-" h
        then raise GetOpt ("Invalid option: " ^ h)
        else parse' t (os, (h :: fs))
in
fun parse args = parse' args (nil, nil)
end

fun main (prog, args) =
    let
        val cmdLine = parse args
    in
(* GetOpt.parse ["-h", "--verbose", "--width", "100", "--height", "200", "f1", "f2"] *)
        OS.Process.success
    end
end
