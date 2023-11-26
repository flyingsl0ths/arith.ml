open Lexer

type parser = {
  output : Lexer.token list;
  operators : Lexer.token list;
  result : (string, Lexer.token list) Either.t;
}

let is_lparen = function Lexer.LParen -> true | _ -> false

let on_comma { operators; _ } =
  List.fold_left
    (fun acc tk -> if is_lparen tk then acc @ [ tk ] else acc)
    [] operators

let on_token_type ({ output; operators; result } as parser') token =
  match token with
  | Lexer.Num _ | Lexer.LParen ->
      { output = output @ [ token ]; operators; result }
  | Lexer.Function _ -> { output; operators = operators @ [ token ]; result }
  | Lexer.Comma -> { output; operators = on_comma parser'; result }
  | _ -> parser'
