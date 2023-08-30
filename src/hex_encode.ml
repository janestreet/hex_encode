(** hex string operations *)

open Core

let to_hex ?(case = `Uppercase) digest =
  let result = Bytes.create (String.length digest * 2) in
  let hex =
    match case with
    | `Uppercase -> "0123456789ABCDEF"
    | `Lowercase -> "0123456789abcdef"
  in
  for i = 0 to String.length digest - 1 do
    let c = int_of_char digest.[i] in
    Bytes.set result (2 * i) hex.[c lsr 4];
    Bytes.set result ((2 * i) + 1) hex.[c land 0xF]
  done;
  Bytes.to_string result
;;

let digit_char_p ?(base = 10) char =
  if base > 36
  then invalid_argf "Hex.digit_char_p(%d,%c): base must be from 0 to 36" base char ();
  let failure () =
    invalid_argf "Hex.digit_char_p: '%c' is not a digit in base %d" char base ()
  in
  let code = int_of_char char in
  let from_zero = code - int_of_char '0' in
  if 0 <= from_zero && from_zero < min base 10
  then from_zero
  else if base > 10
  then (
    let from_a = code - int_of_char 'a' in
    let base_char = base - 10 in
    if 0 <= from_a && from_a < min base_char 26
    then from_a + 10
    else (
      let from_A = code - int_of_char 'A' in
      if 0 <= from_A && from_A < min base_char 26 then from_A + 10 else failure ()))
  else failure ()
;;

let from_hex string =
  let len = String.length string in
  if 0 <> len mod 2
  then invalid_argf "Hex.from_hex(%s): arg length must be even, not %d" string len ();
  let len = len / 2 in
  let res = Bytes.create len in
  let d i = digit_char_p ~base:16 string.[i] in
  for i = 0 to len - 1 do
    Bytes.set res i (char_of_int ((d (2 * i) lsl 4) + d ((2 * i) + 1)))
  done;
  Bytes.to_string res
;;
