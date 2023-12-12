open Arith.Lexer
open Arith.Extensions
open OUnit2

let empty_source _ =
  let lxr = mk_lexer "" in
  let Token (token_column, token), next = lex lxr in
  assert_equal lxr next;
  assert_equal 0 token_column;
  assert_equal End token

let whitespace _ =
  let lxr = mk_lexer "   1" in
  let Token (token_column, token), { source; column; _ } = lex lxr in
  assert_equal "" source;
  assert_equal 4 column;
  assert_equal 3 token_column;
  assert_equal (Num 1.0) token

let whole_number _ =
  let lxr = mk_lexer "1" in
  let Token (token_column, token), { source; column; _ } = lex lxr in
  assert_equal "" source;
  assert_equal 1 column;
  assert_equal 0 token_column;
  assert_equal (Num 1.) token

let decimal _ =
  let lxr = mk_lexer "1.0" in
  let Token (token_column, token), { source; column; _ } = lex lxr in
  assert_equal "" source;
  assert_equal 3 column;
  assert_equal 0 token_column;
  assert_equal (Num 1.0) token

let single_chars _ =
  let cmp_op tk1 tk2 =
    match (tk1, tk2) with
    | ( Function { name = name1; prec = prec1; _ },
        Function { name = name2; prec = prec2; _ } ) ->
        assert_equal name1 name2;
        assert_equal prec1 prec2
    | Operator (c1, prec1, _, asoc1), Operator (c2, prec2, _, asoc2) ->
        assert_equal c1 c2;
        assert_equal prec1 prec2;
        assert_equal asoc1 asoc2
    | LParen, LParen -> assert_bool "" true
    | RParen, RParen -> assert_bool "" true
    | Comma, Comma -> assert_bool "" true
    | _ -> assert_bool "Unexpected comparison" false
  in

  let const n _ = n in

  let type_of_single c (Token (n, tk)) =
    match n with
    | 0 -> (
        match c with
        | '!' ->
            cmp_op
              (Function { name = "!"; f = Unary (fun n -> n); prec = Unary })
              tk
        | '*' -> cmp_op (Operator ('*', Factor, Binary const, true)) tk
        | '^' -> cmp_op (Operator ('^', Factor, Binary const, false)) tk
        | '+' -> cmp_op (Operator ('+', Term, Binary const, true)) tk
        | '/' -> cmp_op (Operator ('/', Factor, Binary const, true)) tk
        | '%' -> cmp_op (Operator ('%', Factor, Binary const, true)) tk
        | '(' -> cmp_op LParen tk
        | ')' -> cmp_op RParen tk
        | ',' -> cmp_op Comma tk
        | c -> assert_bool ("Unknown token " ^ Char.escaped c) false)
    | _ -> assert_bool "Invalid token column" false
  in
  String.iter
    (fun c -> (type_of_single c <<< fst <<< lex <<< mk_lexer) @@ Char.escaped c)
    "!*^+%/(,)"

let double_floating_point _ =
  let lxr = mk_lexer "1..0" in
  let Token (token_column, token), { source; column; _ } = lex lxr in
  assert_equal "1..0" source;
  assert_equal 0 token_column;
  assert_equal 0 column;
  assert_equal
    (Error
       "Syntax error(1,0): floating point number cannot contain more than one \
        '.'")
    token

let cmp_operator expected actual =
  match (expected, actual) with
  | Operator (c1, prec1, _, asoc1), Operator (c2, prec2, _, asoc2) ->
      assert_equal c1 c2;
      assert_equal prec1 prec2;
      assert_equal asoc1 asoc2
  | _ -> assert_bool "Unexpected comparison" false

let cmp_func expected actual =
  match (expected, actual) with
  | ( Function { name = name1; prec = prec1; _ },
      Function { name = name2; prec = prec2; _ } ) ->
      assert_equal name1 name2;
      assert_equal prec1 prec2
  | _ -> assert_bool "Unexpected comparison" false

let functions _ =
  let test_func name =
    let lxr = mk_lexer name in
    let Token (token_column, token), { source; column; _ } = lex lxr in
    assert_equal "" source;
    assert_equal (String.length name) column;
    assert_equal token_column 0;
    cmp_func (Function { name; f = Binary (fun n _ -> n); prec = None }) token
  in
  List.iter test_func
    [
      "abs";
      "acos";
      "acot";
      "acsc";
      "asec";
      "asin";
      "atan";
      "ceil";
      "cos";
      "cosh";
      "cot";
      "csc";
      "exp";
      "exp2";
      "ln";
      "log";
      "log10";
      "rad";
      "round";
      "sec";
      "sin";
      "sinh";
      "sqrt";
      "tan";
      "tanh";
      "deg";
      "floor";
      "nroot";
    ]

let unknown_function _ =
  let lxr = mk_lexer "abc" in
  let Token (token_column, token), { source; column; _ } = lex lxr in
  assert_equal "abc" source;
  assert_equal 0 column;
  assert_equal 0 token_column;
  assert_equal (Error "Unknown function name") token

let unknown_token _ =
  let lxr = mk_lexer ">" in
  let Token (token_column, token), { source; column; _ } = lex lxr in
  assert_equal ">" source;
  assert_equal 0 column;
  assert_equal 0 token_column;
  assert_equal (Error "Unknown token") token

let negative_number _ =
  let lxr = mk_lexer "-10" in
  let Token (token_column, token), ({ source; column; _ } as next) = lex lxr in
  assert_equal "10" source;
  assert_equal 1 column;
  assert_equal 0 token_column;
  cmp_func
    (Function { name = "-"; f = Unary (fun n -> -.n); prec = Unary })
    token;

  let Token (token_column', token'), { source = source'; column = column'; _ } =
    lex next
  in
  assert_equal "" source';
  assert_equal 3 column';
  assert_equal 1 token_column';
  assert_equal (Num 10.0) token'

let negative_num_as_binary_op_operand _ =
  let lxr = mk_lexer "+ -10" in
  let Token (token_column, token), ({ source; column; _ } as next) = lex lxr in
  assert_equal " -10" source;
  assert_equal 1 column;
  assert_equal 0 token_column;
  cmp_operator (Operator ('+', Term, Binary ( +. ), true)) token;

  let ( Token (token_column', token'),
        ({ source = source'; column = column'; _ } as last) ) =
    lex next
  in

  assert_equal "10" source';
  assert_equal 3 column';
  assert_equal 2 token_column';
  cmp_func
    (Function { name = "-"; f = Unary (fun n -> -.n); prec = Unary })
    token';

  let ( Token (token_column'', token''),
        { source = source''; column = column''; _ } ) =
    lex last
  in
  assert_equal "" source'';
  assert_equal 5 column'';
  assert_equal 3 token_column'';
  assert_equal (Num 10.0) token''

let lexer_suite =
  "Lexer"
  >::: [
         "Given An Empty Source" >:: empty_source;
         "Given white space" >:: whitespace;
         "Given a whole number" >:: whole_number;
         "Given a decimal" >:: decimal;
         "Given all single character tokens" >:: single_chars;
         "Given a double floating point number" >:: double_floating_point;
         "Given a function" >:: functions;
         "Given an unknown function name" >:: unknown_function;
         "Given an unknown token" >:: unknown_token;
         "Given a negative number" >:: negative_number;
         "Given a negative number as an operand"
         >:: negative_num_as_binary_op_operand;
       ]

let () = run_test_tt_main lexer_suite
