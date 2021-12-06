(** Implementation for [Predicate.t]. *)

(** A [Predicate] is just a function from ['a] to [bool]. *)

(** {1 Type} *)

type 'a t = 'a -> bool

(** {1 Implementation} *)

(** {2 Contravariant} *)

module Contravariant : Preface_specs.CONTRAVARIANT with type 'a t = 'a t

(** {2 Divisible} *)

module Divisible : Preface_specs.DIVISIBLE with type 'a t = 'a t

(** {2 Decidable} *)

module Decidable : Preface_specs.DECIDABLE with type 'a t = 'a t

(** {2 Invariant Functor} *)

module Invariant : Preface_specs.INVARIANT with type 'a t = 'a t

(** {1 Additional functions}

    Additional functions to facilitate practical work with [Predicate.t]. *)

val negate : 'a t -> 'a t
(** negate the predicate. *)

val tautology : 'a t
(** A predicate always true. *)

val contradiction : 'a t
(** A predicate always false. *)

val and_ : 'a t -> 'a t -> 'a t
(** Compose two predicates (using and). *)

val or_ : 'a t -> 'a t -> 'a t
(** Compose two predicates (using or). *)

(** {2 infix operators} *)

module Infix : sig
  val ( && ) : 'a t -> 'a t -> 'a t
  (** Compose two predicates (using and). *)

  val ( || ) : 'a t -> 'a t -> 'a t
  (** Compose two predicates (using or). *)

  val ( ! ) : 'a t -> 'a t
  (** negate the predicate. *)
end

include module type of Infix
(** @inline *)
