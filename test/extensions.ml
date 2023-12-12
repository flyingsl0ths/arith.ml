open Arith.Extensions
open OUnit2

let ext_until _ = assert_equal 10 @@ until (fun n -> n == 10) (( + ) 1) 0

let array_tail _ =
  assert_equal [] @@ Array.tl [];
  assert_equal [] @@ Array.tl [ 1 ]

let string_hd _ =
  assert_equal ' ' @@ String.hd "";
  assert_equal 'a' @@ String.hd "a"

let string_tail _ =
  assert_equal "" @@ String.tl "";
  assert_equal "" @@ String.tl "a"

let string_null _ =
  assert_equal true @@ String.null "";
  assert_equal false @@ String.null "a"

let string_span _ =
  assert_equal ("", "") @@ String.span (fun _ -> true) "";
  assert_equal ("aaa", "") @@ String.span (fun c -> c == 'a') "aaa";
  assert_equal ("a", ":b") @@ String.span (fun c -> c == 'a') "a:b";
  assert_equal ("", "a:b") @@ String.span (fun c -> c == '-') "a:b"

let string_drop_while _ =
  assert_equal "" @@ String.dropWhile (fun _ -> true) "";
  assert_equal "bc" @@ String.dropWhile (fun c -> c == 'a') "aabc";
  assert_equal "abc" @@ String.dropWhile (fun c -> c == ':') "abc"

let string_drop _ =
  assert_equal "aabc" @@ String.drop 0 "aabc";
  assert_equal "" @@ String.drop 1 "";
  assert_equal "c" @@ String.drop 2 "abc"

let string_take _ =
  assert_equal "" @@ String.take 1 "";
  assert_equal "abc" @@ String.take 4 "abc";
  assert_equal "ab" @@ String.take 2 "abc"

let extensions_suite =
  "Extensions"
  >::: [
         "Extensions.until" >:: ext_until;
         "Array.tl" >:: array_tail;
         "String.tl" >:: string_tail;
         "String.hd" >:: string_hd;
         "String.null" >:: string_null;
         "String.span" >:: string_span;
         "String.dropWhile" >:: string_drop_while;
         "String.drop" >:: string_drop;
         "String.take" >:: string_take;
       ]

let () = run_test_tt_main extensions_suite
