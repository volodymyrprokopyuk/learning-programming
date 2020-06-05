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
    val accept : RegExp.regexp -> string -> bool
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
and parseAtom nil = raise SyntaxError "Factor expected"
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
end;
