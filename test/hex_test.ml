open Core
open Expect_test_helpers_core
module Hex = Hex_encode

let _random_string ?(f = fun () -> char_of_int (Random.int 256)) len =
  let buf = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set buf i (f ())
  done;
  Bytes.to_string buf
;;

let%expect_test "digit_char_p" =
  let test i = print_endline (Int.to_string i) in
  test (Hex.digit_char_p ~base:6 '3');
  [%expect {| 3 |}];
  test (Hex.digit_char_p ~base:16 '3');
  [%expect {| 3 |}];
  test (Hex.digit_char_p ~base:16 'a');
  [%expect {| 10 |}];
  require_does_raise (fun () -> Hex.digit_char_p ~base:7 '7');
  [%expect {| (Invalid_argument "Hex.digit_char_p: '7' is not a digit in base 7") |}];
  require_does_raise (fun () -> Hex.digit_char_p ~base:7 'a');
  [%expect {| (Invalid_argument "Hex.digit_char_p: 'a' is not a digit in base 7") |}];
  require_does_raise (fun () -> Hex.digit_char_p ~base:77 'a');
  [%expect {| (Invalid_argument "Hex.digit_char_p(77,a): base must be from 0 to 36") |}]
;;

let%expect_test "misc" =
  print_endline (Hex.to_hex "abc");
  [%expect {| 616263 |}];
  print_endline (Hex.to_hex ~case:`Uppercase "xyz");
  [%expect {| 78797A |}];
  print_endline (Hex.to_hex ~case:`Lowercase "xyz");
  [%expect {| 78797a |}];
  print_endline (Hex.from_hex "616263");
  [%expect {| abc |}]
;;

let%expect_test "to_hex and from_hex round-trip" =
  Quickcheck.test String.quickcheck_generator ~f:(fun s ->
    let h = Hex.to_hex s in
    if not (String.equal s (Hex.from_hex h)) then failwith (sprintf "hex(%s)" h))
;;
