(** Exposes [Predicate.t], a function from ['a] to [bool].

    {1 Capabilities}

    - {!val:Contravariant} *)

(** {1 Type} *)

type 'a t

(** {1 Implementation} *)

module Contravariant : Preface_specs.CONTRAVARIANT with type 'a t = 'a t
(** {2 Foldable API} *)

(** {1 Helpers} *)

val lift : ('a -> bool) -> 'a t
(** Lift a function from ['a] to [bool] into a predicate. *)

val run : 'a t -> 'a -> bool
(** Run a predicate. *)

(** {1 Predicate modification} *)

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

(** {1 Infix} *)

module Infix : sig
  val ( && ) : 'a t -> 'a t -> 'a t
  (** Compose two predicates (using and). *)

  val ( || ) : 'a t -> 'a t -> 'a t
  (** Compose two predicates (using or). *)

  val ( ! ) : 'a t -> 'a t
  (** negate the predicate. *)
end

include module type of Infix
