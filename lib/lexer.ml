open Extensions

type t = { source : string; column : int }
type token = One of char | OneOrMore of string | Error

let make_lexer source column = { source; column }

let is_single = function
  | '(' -> true
  | ')' -> true
  | '+' -> true
  | '-' -> true
  | '*' -> true
  | '/' -> true
  | '%' -> true
  | '=' -> true
  | _ -> false

let lex { source; column } =
  let len = String.length source in

  let column' = column + 1 in

  let make_one_of c cs len col =
    (One c, make_lexer (String.sub cs 1 len) col)
  in

  match String.hd source with
  | c when is_single c -> make_one_of c source len column'
  | d when Char.is_digit d ->
      let num, rest = String.span (fun c -> not @@ is_single c) source in
      if String.length num = 1 then
        (One (String.hd num), make_lexer rest column')
      else (OneOrMore num, make_lexer rest (column + String.length num))
  | _ -> (Error, { source; column })
