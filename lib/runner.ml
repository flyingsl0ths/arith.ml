open Lexer
open Extensions

type parser_error = ParserError of int * string

type parser = {
  output : Lexer.token list;
  operators : Lexer.token list;
  result : (parser_error, Lexer.token list) Either.t;
}

let had_error { result; _ } = Either.is_left result
let is_lparen = function Lexer.LParen -> true | _ -> false

let plop pred operators output =
  List.fold_left
    (fun ((output', operators') as ctx) token ->
      if pred token then (token :: output', List.tl operators') else ctx)
    (output, operators) operators

let on_comma { output; operators; result } =
  let output', operators' = plop (not <<< is_lparen) operators output in
  { output = output'; operators = operators'; result }

let on_right_paren { output; operators = []; _ } (Lexer.Token (token_column, _))
    =
  {
    output;
    operators = [];
    result = Either.left @@ ParserError (token_column, "Mismatched parenthesis");
  }

let on_right_paren { output; operators; result } (Lexer.Token (token_column, _))
    =
  let output', operators' = plop (not <<< is_lparen) operators output in
  let with_error message =
    {
      output = output';
      operators = operators';
      result = Either.left @@ ParserError (token_column, message);
    }
  in
  match operators' with
  | Comma :: _ -> with_error "Empty argument"
  | LParen :: (Function _ as f) :: operators'' ->
      { output = f :: output'; operators = operators''; result }
  | LParen :: operators'' ->
      { output = output'; operators = operators''; result }
  | _ -> with_error "Mismatched parenthesis"

let precedence_of = function
  | Operator (_, precedence, _, _) -> precedence
  | Function { prec; _ } -> prec
  | _ -> None

let lexeme_of = function
  | Operator (op, _, _, _) -> Char.escaped op
  | Function { name; _ } -> name
  | _ -> ""

let is_op = function
  | Operator _ -> true
  | Function { name; _ } when String.equal name "!" || String.equal name "-" ->
      true
  | _ -> false

let is_left_assoc token =
  let lexeme = lexeme_of token in
  is_op token
  && (not @@ String.equal lexeme "^")
  && (not @@ String.equal lexeme "(")

let on_operator { output; operators; result } token' =
  let precedence_check top_token_prec current_token_prec =
    top_token_prec > current_token_prec
    || (top_token_prec == current_token_prec && is_left_assoc token')
  in
  let remove_from_ops top =
    (not @@ is_lparen top)
    && (precedence_check (precedence_of top) @@ precedence_of token')
  in
  let operators', output' = plop remove_from_ops operators output in
  { output = output'; operators = token' :: operators'; result }

let on_token_type ({ output; operators; result } as parser')
    (Lexer.Token (_, token) as token') =
  match token with
  | Lexer.Num _ | Lexer.LParen ->
      { output = token :: output; operators; result }
  | Lexer.Function _ -> { output; operators = token :: operators; result }
  | Lexer.Comma -> on_comma parser'
  | Lexer.RParen -> on_right_paren parser' token'
  | Lexer.Operator _ -> on_operator parser' token
  | _ -> parser'

(* type t = { source : string; column : int; was_last_token_an_op : bool } *)

let organize lexer =
  let rec parse (lexer', parser') =
    let (Token (_, token) as token'), lexer'' = Lexer.lex lexer' in
    match token with
    | End -> parser'
    | _ ->
        let parser'' = on_token_type parser' token' in
        if had_error parser'' then parser'' else parse (lexer'', parser'')
  in

  let plop_remaing =
    until
      (fun { operators; _ } ->
        List.length operators == 0 || is_lparen (List.hd operators))
      (fun { output; operators; result } ->
        {
          output = List.hd operators :: output;
          operators = List.tl operators;
          result;
        })
  in
  if String.length lexer.source == 0 then Either.right [ Num 0.0 ]
  else
    let { result; _ } =
      plop_remaing
      @@ parse (lexer, { output = []; operators = []; result = Either.right [] })
    in
    result

