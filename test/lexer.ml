open Arith.Lexer
open Arith.Extensions
open OUnit2

let array_tail _ =
  assert_equal [] (Array.tl []);
  assert_equal [] (Array.tl [ 1 ])

let string_hd _ =
  assert_equal ' ' (String.hd "");
  assert_equal 'a' (String.hd "a")

let string_tail _ =
  assert_equal "" (String.tl "");
  assert_equal "" (String.tl "a")

let string_null _ =
  assert_equal true (String.null "");
  assert_equal false (String.null "a")

let string_span _ =
  assert_equal ("", "") (String.span (fun _ -> true) "");
  assert_equal ("aaa", "") (String.span (fun c -> c == 'a') "aaa");
  assert_equal ("a", ":b") (String.span (fun c -> c == 'a') "a:b");
  assert_equal ("", "a:b") (String.span (fun c -> c == '-') "a:b")

let string_drop_while _ =
  assert_equal "" (String.dropWhile (fun _ -> true) "");
  assert_equal "bc" (String.dropWhile (fun c -> c == 'a') "aabc");
  assert_equal "abc" (String.dropWhile (fun c -> c == ':') "abc")

let string_drop _ =
  assert_equal "aabc" (String.drop 0 "aabc");
  assert_equal "" (String.drop 1 "");
  assert_equal "c" (String.drop 2 "abc")

let string_take _ =
  assert_equal "" (String.take 1 "");
  assert_equal "abc" (String.take 4 "abc");
  assert_equal "ab" (String.take 2 "abc")

let empty_source _ =
  let lxr = mk_lexer "" in
  let token, next = lex lxr in
  assert_equal lxr next;
  assert_equal End token

let whitespace _ =
  let lxr = mk_lexer "   1" in
  let token, { source; column; _ } = lex lxr in
  assert_equal "" source;
  assert_equal 4 column;
  assert_equal (Num 1.0) token

let whole_number _ =
  let lxr = mk_lexer "1" in
  let token, { source; column; _ } = lex lxr in
  assert_equal "" source;
  assert_equal 1 column;
  assert_equal (Num 1.) token

let decimal _ =
  let lxr = mk_lexer "1.0" in
  let token, { source; column; _ } = lex lxr in
  assert_equal "" source;
  assert_equal 3 column;
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

  let type_of_single c tk =
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
    | c -> assert_bool ("Unknown token " ^ Char.escaped c) false
  in
  String.iter
    (fun c -> (type_of_single c <<< fst <<< lex <<< mk_lexer) @@ Char.escaped c)
    "!*^+%/(,)"

let double_floating_point _ =
  let lxr = mk_lexer "1..0" in
  let token, { source; column; _ } = lex lxr in
  assert_equal "1..0" source;
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
    let token, { source; column; _ } = lex lxr in
    assert_equal "" source;
    assert_equal (String.length name) column;
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
  let token, { source; column; _ } = lex lxr in
  assert_equal "abc" source;
  assert_equal 0 column;
  assert_equal (Error "Unknown function name") token

let unknown_token _ =
  let lxr = mk_lexer ">" in
  let token, { source; column; _ } = lex lxr in
  assert_equal ">" source;
  assert_equal 0 column;
  assert_equal (Error "Unknown token") token

let negative_number _ =
  let lxr = mk_lexer "-10" in
  let token, ({ source; column; _ } as next) = lex lxr in
  assert_equal "10" source;
  assert_equal 1 column;
  cmp_func
    (Function { name = "-"; f = Unary (fun n -> -.n); prec = Unary })
    token;

  let token', { source = source'; column = column'; _ } = lex next in
  assert_equal "" source';
  assert_equal 3 column';
  assert_equal (Num 10.0) token'

let negative_num_as_binary_op_operand _ =
  let lxr = mk_lexer "+ -10" in
  let token, ({ source; column; _ } as next) = lex lxr in
  assert_equal " -10" source;
  assert_equal 1 column;
  cmp_operator (Operator ('+', Term, Binary ( +. ), true)) token;

  let token', ({ source = source'; column = column'; _ } as last) = lex next in

  assert_equal "10" source';
  assert_equal 3 column';
  cmp_func
    (Function { name = "-"; f = Unary (fun n -> -.n); prec = Unary })
    token';

  let token'', { source = source''; column = column''; _ } = lex last in
  assert_equal "" source'';
  assert_equal 5 column'';
  assert_equal (Num 10.0) token''

let extensions_suite =
  "Extensions"
  >::: [
         "Array.tl" >:: array_tail;
         "String.tl" >:: string_tail;
         "String.hd" >:: string_hd;
         "String.null" >:: string_null;
         "String.span" >:: string_span;
         "String.dropWhile" >:: string_drop_while;
         "String.drop" >:: string_drop;
         "String.take" >:: string_take;
       ]

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

let () =
  run_test_tt_main extensions_suite;
  run_test_tt_main lexer_suite
