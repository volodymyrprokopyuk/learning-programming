(* Usage: [-h | --help] [-v | --verbose] --width width --height hieght files *)
structure GetOpt =
struct

type cmdopt = string * string option
type cmdline = (cmdopt list) * (string list)

exception GetOpt of string

local
    fun parse' nil r = r
      (* OR pattern *)
      | parse' (("-h" | "--help") :: t) (os, fs) =
        parse' t (("help", NONE) :: os, fs)
      | parse' (("-v" | "--verbose") :: t) (os, fs) =
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
fun parse args : cmdline = parse' args (nil, nil)
end

fun requireOption name (os, _) =
    let
        val res = List.find (fn (k, _) => k = name) os
    in
        case res
            of NONE => raise GetOpt ("Missing mandatory option: " ^ name)
            | SOME opt => opt
    end

fun requireOptionValue name (os, _) =
    let
        val res = List.find (fn (k, _) => k = name) os
    in
        case res
         of NONE => raise GetOpt ("Missing mandatory option: " ^ name)
          | SOME (_, value) =>
            case value
             of NONE => raise GetOpt ("Missing value for option: " ^ name)
              | SOME v => v
    end

fun main (prog, args) =
    let
        val cmdLine = parse args
        val help = requireOption "help" cmdLine
        val width = requireOptionValue "width" cmdLine
    in
        OS.Process.success
    end
end;

let
    open GetOpt
    val args = ["--help", "--verbose", "f0", "--width", "100", "--height", "200",
                "f1", "f2"]
    val cmdLine = parse args
    val help = requireOption "help" cmdLine
    val width = requireOptionValue "width" cmdLine
in
    print "Success\n"
end
handle GetOpt.GetOpt msg => print ("Failure: " ^ msg ^ "\n")
