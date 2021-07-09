(** A law is characterised by a [name], a left-hand side of an equation ([lhs])
    and a right-hand side [rhs], i.e. two functions of the same type. To verify
    a law, it is sufficient to check that both parts of the equation return the
    same value for the same input. *)

(** {1 Description of a part of an equation}*)

module Side : sig
  type ('a, 'b) t
  (** The type held by the side of the equation. *)

  val make : string -> ('a -> 'b) -> ('a, 'b) t
  (** Promote a function into an equation part with a representation. *)

  val repr : ('a, 'b) t -> string
  (** Get the representation of an equation part. *)

  val fun_ : ('a, 'b) t -> 'a -> 'b
  (** Extract the function of the equation part.*)

  val pp : Format.formatter -> ('a, 'b) t -> unit
  (** Pretty printer for equation part. *)
end

(** {1 Description of a Law equation}*)

type ('a, 'b) t
(** Describes an equation with a name and two part ([lhs] and [rhs]). *)

val make : string -> ('a, 'b) Side.t -> ('a, 'b) Side.t -> ('a, 'b) t
(** Given a [name] and two side of the equation it produce a nammed equation. *)

val name : ('a, 'b) t -> string
(** Get the name of the law. *)

val lhs : ('a, 'b) t -> ('a, 'b) Side.t
(** Retreive the left hand side of the equation. *)

val rhs : ('a, 'b) t -> ('a, 'b) Side.t
(** Retreive the right hand side of the equation. *)

val pp : Format.formatter -> ('a, 'b) t -> unit
(** Pretty printer for a complete equation. *)
