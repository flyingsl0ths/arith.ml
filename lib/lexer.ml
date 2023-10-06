open Extensions
module Funcs = Map.Make (String)

type t = { source : string; column : int; was_last_token_an_op : bool }

type precedence =
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
  | Function of { name : string; f : relation }
  | LParen
  | RParen
  | Comma
  | Eof
  | Error of string

let lexer source = { source; column = 0; was_last_token_an_op = false }

let funcs =
  let ( <<< ) f g x = f @@ g x in
  let recip n = 1.0 /. n in
  let to_radians x = x *. (Float.pi /. 180.) in
  let to_degrees x = x *. (180. /. Float.pi) in
  Funcs.add "nroot"
    (Function { name = "nroot"; f = Binary (fun x y -> Float.pow x @@ recip y) })
  @@ Funcs.add "floor" (Function { name = "floor"; f = Unary Float.floor })
  @@ Funcs.add "deg" (Function { name = "deg"; f = Unary to_degrees })
  @@ Funcs.add "tan" (Function { name = "tan"; f = Unary Float.tan })
  @@ Funcs.add "sqrt" (Function { name = "sqrt"; f = Unary Float.sqrt })
  @@ Funcs.add "sinh" (Function { name = "sinh"; f = Unary Float.sinh })
  @@ Funcs.add "sin" (Function { name = "sin"; f = Unary Float.sin })
  @@ Funcs.add "sec"
       (Function { name = "sec"; f = Unary (recip <<< Float.cos) })
  @@ Funcs.add "round" (Function { name = "round"; f = Unary Float.round })
  @@ Funcs.add "rad" (Function { name = "rad"; f = Unary to_radians })
  @@ Funcs.add "log10" (Function { name = "log10"; f = Unary Float.log10 })
  @@ Funcs.add "log"
       (Function
          {
            name = "log";
            f = Binary (fun n m -> Float.log @@ (n /. Float.log m));
          })
  @@ Funcs.add "ln" (Function { name = "ln"; f = Unary Float.log })
  @@ Funcs.add "exp2" (Function { name = "exp2"; f = Unary Float.exp2 })
  @@ Funcs.add "exp" (Function { name = "exp"; f = Unary Float.exp })
  @@ Funcs.add "csc"
       (Function { name = "csc"; f = Unary (recip <<< Float.sin) })
  @@ Funcs.add "cot"
       (Function { name = "cot"; f = Unary (recip <<< Float.tan) })
  @@ Funcs.add "cosh" (Function { name = "cosh"; f = Unary Float.cosh })
  @@ Funcs.add "cos" (Function { name = "cos"; f = Unary Float.cos })
  @@ Funcs.add "ceil" (Function { name = "ceil"; f = Unary Float.ceil })
  @@ Funcs.add "atan" (Function { name = "atan"; f = Unary Float.atan })
  @@ Funcs.add "asin" (Function { name = "asin"; f = Unary Float.asin })
  @@ Funcs.add "asec"
       (Function { name = "asec"; f = Unary (fun x -> Float.acos (1.0 /. x)) })
  @@ Funcs.add "acsc"
       (Function { name = "acsc"; f = Unary (fun x -> Float.asin (1.0 /. x)) })
  @@ Funcs.add "acot"
       (Function { name = "acot"; f = Unary (fun x -> Float.atan (1.0 /. x)) })
  @@ Funcs.add "acos" (Function { name = "acos"; f = Unary Float.acos })
  @@ Funcs.add "abs"
       (Function { name = "abs"; f = Unary Float.abs })
       Funcs.empty

let discard = function ' ' -> true | '\t' -> true | _ -> false

let skip_whitespace { source; column; was_last_token_an_op } =
  let trimmed = String.dropWhile discard source in
  {
    source = trimmed;
    column = column + abs (String.length trimmed - String.length source);
    was_last_token_an_op;
  }

let factorial = function
  | 1.0 -> 1.0
  | n ->
      let rec factorial' acc = function
        | 1.0 -> acc
        | n' -> factorial' (n' -. 1.0) (n' *. acc)
      in
      factorial' n 1.0

let is_left_assoc c =
  match c with '+' | '-' | '*' | '/' | '%' | '!' -> true | _ -> false

let single_char_token is_unary_minus = function
  | '+' -> Operator ('+', Term, Binary ( +. ), is_left_assoc '+')
  | '*' -> Operator ('*', Factor, Binary ( *. ), is_left_assoc '*')
  | '/' -> Operator ('/', Factor, Binary ( /. ), is_left_assoc '/')
  | '%' -> Operator ('%', Factor, Binary mod_float, is_left_assoc '%')
  | '^' -> Operator ('^', Factor, Binary (fun x y -> x ** y), is_left_assoc '^')
  | '-' when not is_unary_minus ->
      Operator ('-', Term, Binary ( -. ), is_left_assoc '-')
  | '-' when is_unary_minus ->
      Operator ('-', Unary, Unary (fun n -> -.n), is_left_assoc '-')
  | '!' -> Operator ('!', Unary, Unary factorial, is_left_assoc '!')
  | '(' -> LParen
  | ')' -> RParen
  | ',' -> Comma
  | _ -> Eof

let make_func ({ source; column; _ } as lexer) fs =
  match
    List.find_opt (fun f -> String.take (String.length f) source == f) fs
  with
  | Some f ->
      let offset = String.length f in
      ( Funcs.find f funcs,
        {
          source = String.drop offset source;
          column = column + offset;
          was_last_token_an_op = false;
        } )
  | None -> (Error "Unknown function name", lexer)

let match_functions_or_error lexer = function
  | 'a' ->
      make_func lexer [ "abs"; "acos"; "acot"; "acsc"; "asec"; "asin"; "atan" ]
  | 'c' -> make_func lexer [ "ceil"; "cosh"; "cos"; "cot"; "csc" ]
  | 'e' -> make_func lexer [ "exp2"; "exp" ]
  | 'l' -> make_func lexer [ "ln"; "log10"; "log" ]
  | 'r' -> make_func lexer [ "rad"; "round" ]
  | 's' -> make_func lexer [ "sec"; "sinh"; "sin"; "sqrt" ]
  | 't' -> make_func lexer [ "tanh"; "tan" ]
  | 'd' -> make_func lexer [ "deg" ]
  | 'f' -> make_func lexer [ "floor" ]
  | 'n' -> make_func lexer [ "nroot" ]
  | _ -> (Error "Unknown token", lexer)

let is_single = function
  | '+' | '-' | '*' | '/' | '%' | '^' | '!' | '(' | ')' | ',' -> true
  | _ -> false

let is_op = function
  | '+' | '-' | '*' | '/' | '%' | '^' | '!' -> true
  | _ -> false

let peek cs = String.hd @@ String.tl cs

let is_unary_minus { source; column; was_last_token_an_op; _ } =
  let first = String.hd source in
  (was_last_token_an_op && first == '-')
  || (first == '-' && peek source == '(')
  || (column == 0 && first == '-')

let parse_num source column =
  let num, rest = String.span (fun c -> Char.is_digit c || c = '.') source in
  let count_decimals =
    String.fold_left
      (fun count current -> if current == '.' then count + 1 else count)
      0
  in
  let total_decimals = count_decimals num in
  let tk =
    if total_decimals > 1 then
      Error
        ("Syntax error(1," ^ string_of_int column
       ^ "): floating point number cannot contain more than one '.'")
    else Num (float_of_string num)
  in
  ( tk,
    {
      source = rest;
      column = column + String.length num;
      was_last_token_an_op = false;
    } )

let rec lex ({ source; column; _ } as lexer) =
  if String.null source then (Eof, lexer)
  else
    let column' = column + 1 in
    match String.hd source with
    | c when discard c -> lex @@ skip_whitespace lexer
    | c when is_single c ->
        ( single_char_token (is_unary_minus lexer) c,
          {
            source = String.tl source;
            column = column';
            was_last_token_an_op = is_op c;
          } )
    | d when Char.is_digit d -> parse_num source column
    | c -> match_functions_or_error lexer c
