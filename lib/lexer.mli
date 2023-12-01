type t = private { source : string; column : int; was_last_token_an_op : bool }

type precedence =
  | None
  | Term
  (*  + - *)
  | Factor
  (* * / % ^ *)
  | Unary (* ! - *)

type relation =
  | Unary of (float -> float)
  | Binary of (float -> float -> float)

type token =
  | Num of float
  | Operator of char * precedence * relation * bool
  | Function of { name : string; f : relation; prec : precedence }
  | LParen
  | RParen
  | Comma
  | End
  | Error of string

type numbered_token = Token of int * token

val lex : t -> numbered_token * t
val mk_lexer : string -> t
