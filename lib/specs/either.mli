(** [Either.t] represents values with two possibilities. *)

type ('a, 'b) t =
  | Left of 'a
  | Right of 'b
