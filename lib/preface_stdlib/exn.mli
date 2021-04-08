(** Built-in exception used along [Preface]. *)

(** {1 Types} *)

type t = exn
(** Alias of [exn] as [Exn.t]. *)

(** {1 Exceptions}

    Built-in exceptions. *)

exception Negative_position of int
(** Occurs when an integer should be positive. *)

(** {1 Addtional functions}

    Additional functions to facilitate practical work with [Exn.t]. *)

val check_position : int -> (int, t) result
(** Check if a position is positive. *)

val equal : t -> t -> bool
(** Equality between [Exn.t].*)

val pp : Format.formatter -> t -> unit
(** Formatter for pretty-printing for [Exn.t]. *)
