(* Regular expression engine *)
signature REGEXP =
sig
    (* Abstract syntax for regular expressions *)
    datatype regexp = Zero | One | Char of char |
                      Plus of regexp * regexp |
                      Times of regexp * regexp |
                      Star of regexp
    exception SyntaxError of string
    val parse : string -> regexp
    val format : regexp -> string
end;

(* Regular expression matcher *)
signature MATCHER =
sig
    (* MATCHER depends on REGEXP (substructure specification) *)
    structure RegExp : REGEXP
    (* Curried function *)
    val createMatcher : RegExp.regexp -> string -> bool
end;

structure RegExp :> REGEXP =
struct

datatype regexp = Zero | One | Char of char |
                  Plus of regexp * regexp |
                  Times of regexp * regexp |
                  Star of regexp

exception SyntaxError of string

exception LexicalError

(* Tokens are abstract symbols *)
datatype token = PlusSign | TimesSign | Asterisk | LParen | RParen |
                 AtSign | Percent | Literal of char

(* Translate characters into tokens (abstact symbols) *)
fun tokenize nil = nil
  | tokenize (#"+" :: cs) = PlusSign :: tokenize cs
  | tokenize (#"." :: cs) = TimesSign :: tokenize cs
  | tokenize (#"*" :: cs) = Asterisk :: tokenize cs
  | tokenize (#"(" :: cs) = LParen :: tokenize cs
  | tokenize (#")" :: cs) = RParen :: tokenize cs
  | tokenize (#"@" :: cs) = AtSign :: tokenize cs
  | tokenize (#"%" :: cs) = Percent :: tokenize cs
  | tokenize (#"\\" :: c :: cs) = Literal c :: tokenize cs
  | tokenize (#"\\" :: nil) = raise LexicalError
  | tokenize (#" " :: cs) = tokenize cs
  | tokenize (c :: cs) = Literal c :: tokenize cs

(* Recursive-descent parser implemented as a set of mutually recursive functions *)
(* Regexp grammar *)
(*     rexp := rterm | rterm+rexp *)
(*     rterm := rfac | rfac.rterm *)
(*     rfac := ratom | ratom* *)
(*     ratom := @ | % | a | (rexp) *)
fun parseExp ts =
    let
        val (r, ts') = parseTerm ts
    in
        case ts' of
            (PlusSign :: ts'') =>
            let
                val (r', ts''') = parseExp ts''
            in
                (Plus (r, r'), ts''')
            end
          | _ => (r, ts')
    end
and parseTerm ts =
    let
        val (r, ts') = parseFactor ts
    in
        case ts' of
            (TimesSign :: ts'') =>
            let
                val (r', ts''') = parseTerm ts''
            in
                (Times (r, r'), ts''')
            end
          | _ => (r, ts')
    end
and parseFactor ts =
    let
        val (r, ts') = parseAtom ts
    in
        case ts' of
            (Asterisk :: ts'') => (Star r, ts'')
          | _ => (r, ts')
    end
and parseAtom nil = raise SyntaxError "Atom expected"
  | parseAtom (AtSign :: ts) = (Zero, ts)
  | parseAtom (Percent :: ts) = (One, ts)
  | parseAtom ((Literal c) :: ts) = (Char c, ts)
  | parseAtom (LParen :: ts) =
    let
        val (r, ts') = parseExp ts
    in
        case ts' of
            nil => raise SyntaxError "Right parenthesis exprected"
          | (RParen :: ts'') => (r, ts'')
          | _ => raise SyntaxError "Right parenthesis expected"
    end

fun parse s =
    let
        val (r, ts) = parseExp (tokenize (String.explode s))
    in
        case ts of
            nil => r
         | _ => raise SyntaxError "Invalid regexp"
    end
    handle LexicalError => raise SyntaxError "Invalid pattern"

fun formatExp Zero = [#"@"]
  | formatExp One =  [#"%"]
  | formatExp (Char c) = [c]
  | formatExp (Plus (r1, r2)) =
    let
        val s1 = formatExp r1
        val s2 = formatExp r2
    in
        [#"("] @ s1 @ [#"+"] @ s2 @ [#")"]
    end
  | formatExp (Times (r1, r2)) =
    let
        val s1 = formatExp r1
        val s2 = formatExp r2
    in
        s1 @ [#"*"] @ s2
    end
  | formatExp (Star r) =
    let
        val s = formatExp r
    in
        [#"("] @ s @ [#")"] @ [#"*"]
    end

fun format r = String.implode (formatExp r)

end;

structure Matcher :> MATCHER =
struct

structure RegExp = RegExp
open RegExp

(* Recursive analysis of the regular expression *)
(* Matcher matches some initial segment of a string, *)
(* then passes the remaining segment of a string to a continuation *)
fun match Zero _ k = false
  | match One cs k = k cs
  | match (Char c) cs k =
    (case cs of
         nil => false
       | c' :: cs' => (c = c') andalso (k cs'))
  (* Alternative: one or another *)
  | match (Plus (r1, r2)) cs k =
    (match r1 cs k) orelse (match r2 cs k)
  (* Concatenation: one and another (via continuation) *)
  | match (Times (r1, r2)) cs k =
    match r1 cs (fn cs' => match r2 cs' k)
  (* Iteration: zero or more (via continuation) *)
  | match (r as Star r1) cs k =
    (k cs) orelse match r1 cs (fn cs' => match r cs' k)

(* Lambda is the initial continuation that ensure that *)
(* the remaining input is empty to determine the match resutl *)
fun createMatcher r s = match r (String.explode s) (fn nil => true | _ => false)

end;

(* Test RegExp parse and format *)
(* let *)
(*     open RegExp *)
(*     (* val p = "((a))" *) *)
(*     (* val p = "@" *) *)
(*     (* val p = "%" *) *)
(*     (* val p = "a+b" *) *)
(*     (* val p = "a.b" *) *)
(*     (* val p = "a*" *) *)
(*     val p = "(a+b)*" *)
(*     val r = parse p *)
(*     val s = format r *)
(* in *)
(*     s *)
(* end; *)

(* Test Matcher *)
let
    open Matcher Matcher.RegExp
    (* val p = "(a+b)*" *)
    (* val s = "aababb" *)
    (* val p = "(a.b)*" *)
    (* val s = "abab" *)
    val p = "\\*"
    val s = "*"
    val match = createMatcher (parse p)
in
    match s
end;
