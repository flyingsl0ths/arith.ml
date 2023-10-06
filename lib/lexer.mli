type t = private { source : string; column : int; was_last_token_an_op : bool }

type precedence = private
  | Term
  (*  + - *)
  | Factor
  (* * / % ^ *)
  | Unary (* ! - *)

type relation = private
  | Unary of (float -> float)
  | Binary of (float -> float -> float)

type token = private
  | Num of float
  | Operator of char * precedence * relation * bool
  | Function of { name : string; f : relation }
  | LParen
  | RParen
  | Comma
  | Eof
  | Error of string

val lex : t -> token * t
val lexer : string -> t
