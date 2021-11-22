(** A specialized version of a [Store comonad] with [Identity] as the inner
    comonad. Since [Preface.Make.Store] is a Transformer, this module exposes
    the classical Store comonad (or [Costate]). *)

(** {1 Specialization}

    A [Store Comonad] is set up by a type. *)

module Over (Store : Preface_specs.Types.T0) : sig
  include Preface_specs.STORE with type store = Store.t
  (** @inline*)

  val store : (store -> 'a) -> store -> 'a t
  (** Create a store using an accessor and a stored value. *)

  val run_identity : 'a t -> (store -> 'a) * store
  (** Run the store through the [identity].*)

  (** {2 Functor} *)

  module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t

  (** {2 Invariant} *)

  module Invariant : Preface_specs.INVARIANT with type 'a t = 'a t
end
