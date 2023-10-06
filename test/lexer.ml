(* open Arith.Lexer *)
open OUnit2

let unity x = x
let test1 _ = assert_equal "x" @@ unity "x"
let test2 _ = assert_equal 100 @@ unity 100
let suite = "Suite" >::: [ "test1" >:: test1; "test2" >:: test2 ]
let () = run_test_tt_main suite
