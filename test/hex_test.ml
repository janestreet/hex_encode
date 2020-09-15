open OUnit
open Core_kernel
open Poly

module Hex = Hex_encode

let random_string ?(f = (fun () -> char_of_int (Random.int 256))) len =
  let buf = Bytes.create len in
  for i = 0 to len-1 do Bytes.set buf i (f ()); done;
  Bytes.to_string buf

let test = "hex" >::: [
  "digit_char_p" >:: (fun () ->
    "3/6" @? ((Hex.digit_char_p ~base:6 '3') = 3);
    "3/16" @? ((Hex.digit_char_p ~base:16 '3') = 3);
    "a/16" @? ((Hex.digit_char_p ~base:16 'a') = 10);
    "7/7" @? (try ignore (Hex.digit_char_p ~base:7 '7'); false
      with Invalid_argument _ -> true);
    "a/7" @? (try ignore (Hex.digit_char_p ~base:7 'a'); false
      with Invalid_argument _ -> true);
    "a/77" @? (try ignore (Hex.digit_char_p ~base:77 'a'); false
      with Invalid_argument _ -> true);
  );
  "misc" >:: (fun () ->
    "abc" @? ((Hex.to_hex "abc") = "616263");
    "xyz-uc" @? ((Hex.to_hex ~case:`Uppercase "xyz") = "78797A");
    "xyz-lc" @? ((Hex.to_hex ~case:`Lowercase "xyz") = "78797a");
    "616263" @? ((Hex.from_hex "616263") = "abc");
    "random" @? (for _i = 1 to 100 do
                   let len = 10 + (Random.int 100) in
                   for _ = 1 to 100 do
                     let s = random_string len in
                     let h = Hex.to_hex s in
                     if s <> Hex.from_hex h then failwith
                       (sprintf "hex(%s)" h);
                   done;
                 done;
                 true));
]
