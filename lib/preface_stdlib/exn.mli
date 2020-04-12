(** Built-in exception used along [Preface]. *)

(** {1 Types} *)

type t = exn
(** Alias of [exn] as [Exn.t]. *)

(** {1 Exceptions}

    Built-in exceptions. *)

exception Negative_position of int
(** Occure when an integer should be positive. *)

(** {1 Helpers} *)

val check_position : int -> int Try.t
(** Check if a position is positive. *)
