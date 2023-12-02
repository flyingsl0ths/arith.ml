open Lexer

type parser = {
  output : Lexer.token list;
  operators : Lexer.token list;
  result : (string, Lexer.token list) Either.t;
}

let is_lparen = function Lexer.LParen -> true | _ -> false

let plop operators output =
  List.fold_left
    (fun ((output', operators') as ctx) token ->
      if not @@ is_lparen token then (token :: output', List.tl operators')
      else ctx)
    (output, operators) operators

let on_comma { output; operators; result } =
  let output', operators' = plop operators output in
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
  let output', operators' = plop operators output in
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

let on_token_type ({ output; operators; result } as parser')
    (Lexer.Token (_, token) as token') =
  match token with
  | Lexer.Num _ | Lexer.LParen ->
      { output = token :: output; operators; result }
  | Lexer.Function _ -> { output; operators = token :: operators; result }
  | Lexer.Comma -> on_comma parser'
  | Lexer.RParen -> on_right_paren parser' token'
  | _ -> parser'
