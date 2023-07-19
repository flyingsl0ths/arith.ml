type t
type token = private One of char | OneOrMore of string | Error

val make_lexer : string -> int -> t
val lex : t -> token * t
