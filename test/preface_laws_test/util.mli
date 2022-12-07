(** Utility to facilitate the writing of a test suite. *)

val pp_of_print : ('a -> string) -> Format.formatter -> 'a -> unit

val with_alcotest :
     ?colors:bool
  -> ?verbose:bool
  -> ?long:bool
  -> count:int
  -> (string * (count:int -> QCheck2.Test.t list)) list
  -> (string * unit Alcotest.test_case list) list
