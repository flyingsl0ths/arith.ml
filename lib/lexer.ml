open Extensions
module Funcs = Map.Make (String)

type t = { source : string; column : int; was_last_token_an_op : bool }

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

let mk_lexer source = { source; column = 0; was_last_token_an_op = false }
let no_prec_func name f = Function { name; f; prec = None }

let funcs =
  let recip n = 1.0 /. n in
  let to_radians x = x *. (Float.pi /. 180.) in
  let to_degrees x = x *. (180. /. Float.pi) in
  Funcs.add "nroot"
    (no_prec_func "nroot" @@ Binary (fun x y -> Float.pow x @@ recip y))
  @@ Funcs.add "floor" (no_prec_func "floor" @@ Unary Float.floor)
  @@ Funcs.add "deg" (no_prec_func "deg" @@ Unary to_degrees)
  @@ Funcs.add "tanh" (no_prec_func "tanh" @@ Unary Float.tanh)
  @@ Funcs.add "tan" (no_prec_func "tan" @@ Unary Float.tan)
  @@ Funcs.add "sqrt" (no_prec_func "sqrt" @@ Unary Float.sqrt)
  @@ Funcs.add "sinh" (no_prec_func "sinh" @@ Unary Float.sinh)
  @@ Funcs.add "sin" (no_prec_func "sin" @@ Unary Float.sin)
  @@ Funcs.add "sec" (no_prec_func "sec" @@ Unary (recip <<< Float.cos))
  @@ Funcs.add "round" (no_prec_func "round" @@ Unary Float.round)
  @@ Funcs.add "rad" (no_prec_func "rad" @@ Unary to_radians)
  @@ Funcs.add "log10" (no_prec_func "log10" @@ Unary Float.log10)
  @@ Funcs.add "log"
       (no_prec_func "log"
       @@ Binary (fun n m -> Float.log @@ (n /. Float.log m)))
  @@ Funcs.add "ln" (no_prec_func "ln" @@ Unary Float.log)
  @@ Funcs.add "exp2" (no_prec_func "exp2" @@ Unary Float.exp2)
  @@ Funcs.add "exp" (no_prec_func "exp" @@ Unary Float.exp)
  @@ Funcs.add "csc" (no_prec_func "csc" @@ Unary (recip <<< Float.sin))
  @@ Funcs.add "cot" (no_prec_func "cot" @@ Unary (recip <<< Float.tan))
  @@ Funcs.add "cosh" (no_prec_func "cosh" @@ Unary Float.cosh)
  @@ Funcs.add "cos" (no_prec_func "cos" @@ Unary Float.cos)
  @@ Funcs.add "ceil" (no_prec_func "ceil" @@ Unary Float.ceil)
  @@ Funcs.add "atan" (no_prec_func "atan" @@ Unary Float.atan)
  @@ Funcs.add "asin" (no_prec_func "asin" @@ Unary Float.asin)
  @@ Funcs.add "asec"
       (no_prec_func "asec" @@ Unary (fun x -> Float.acos (1.0 /. x)))
  @@ Funcs.add "acsc"
       (no_prec_func "acsc" @@ Unary (fun x -> Float.asin (1.0 /. x)))
  @@ Funcs.add "acot"
       (no_prec_func "acot" @@ Unary (fun x -> Float.atan (1.0 /. x)))
  @@ Funcs.add "acos" (no_prec_func "acos" @@ Unary Float.acos)
  @@ Funcs.add "abs" (no_prec_func "abs" @@ Unary Float.abs) Funcs.empty

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
      Function { name = "-"; f = Unary (fun n -> -.n); prec = Unary }
  | '!' -> Function { name = "!"; f = Unary factorial; prec = Unary }
  | '(' -> LParen
  | ')' -> RParen
  | ',' -> Comma
  | _ -> End

let make_func ({ source; column; _ } as lexer) fs =
  let source_len = String.length source in
  match
    List.find_opt
      (fun f ->
        let func_name_length = String.length f in
        if source_len < func_name_length then false
        else String.equal f @@ String.sub source column (String.length f))
      fs
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

let match_functions_or_error ({ source; column; _ } as lexer) =
  let is_not_name { source; column; _ } func_name =
    let source_len = String.length source in
    let func_name_length = String.length func_name in
    source_len >= func_name_length
    && not
       @@ String.equal
            (String.sub source column (column + func_name_length))
            func_name
  in
  let is_not_exp_functions lexer =
    is_not_name lexer "exp" || is_not_name lexer "exp2"
  in
  function
  | 'a' ->
      make_func lexer [ "abs"; "acos"; "acot"; "acsc"; "asec"; "asin"; "atan" ]
  | 'c' -> make_func lexer [ "ceil"; "cosh"; "cos"; "cot"; "csc" ]
  | 'e' when String.length source == 1 || is_not_exp_functions lexer ->
      ( Num 2.71828182845904523536028747135266250,
        {
          source = String.tl source;
          column = column + 1;
          was_last_token_an_op = false;
        } )
  | 'e' -> make_func lexer [ "exp2"; "exp" ]
  | 'l' -> make_func lexer [ "ln"; "log10"; "log" ]
  | 'p' ->
      ( Num Float.pi,
        {
          source = String.drop 2 source;
          column = column + 2;
          was_last_token_an_op = false;
        } )
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
  || column == 0
     && String.length source >= 2
     &&
     let second = (String.hd <<< String.tl <<< String.tl) source in
     Char.is_digit second || second == '-'

let parse_num ({ source; column; _ } as lexer) =
  let num, rest = String.span (fun c -> Char.is_digit c || c = '.') source in
  let count_decimals =
    String.fold_left
      (fun count current -> if current == '.' then count + 1 else count)
      0
  in
  if count_decimals num > 1 then
    ( Token
        ( column,
          Error
            ("Syntax error(1," ^ string_of_int column
           ^ "): floating point number cannot contain more than one '.'") ),
      lexer )
  else
    ( Token (column, Num (float_of_string num)),
      {
        source = rest;
        column = column + String.length num;
        was_last_token_an_op = false;
      } )

let rec lex ({ source; column; _ } as lexer) =
  if String.null source then (Token (column, End), lexer)
  else
    match String.hd source with
    | c when discard c -> lex @@ skip_whitespace lexer
    | c when is_single c ->
        ( Token (column, single_char_token (is_unary_minus lexer) c),
          {
            source = String.tl source;
            column = column + 1;
            was_last_token_an_op = is_op c;
          } )
    | d when Char.is_digit d -> parse_num lexer
    | c ->
        let tk, lexer' = match_functions_or_error lexer c in
        (Token (column, tk), lexer')
