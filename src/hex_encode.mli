@@ portable

(** encode strings in hex, e.g., "abc" <--> "616263" *)

(** encode string: "abc" --> "616263" *)
val to_hex : ?case:[ `Uppercase | `Lowercase ] -> string -> string

(** interpret the character as a digit in the given base *)
val digit_char_p : ?base:int -> char -> int

(** decode string: "616263" --> "abc" *)
val from_hex : string -> string
