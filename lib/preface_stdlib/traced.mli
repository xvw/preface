(** A specialized version of a [Traced comonad] with [Identity] as the inner
    comonad. Since [Preface.Make.Traced] is a Transformer, this module exposes
    the classical Traced comonad (or [Cowriter]). *)

(** {1 Specialization}

    A [Traced Comonad] is set up by a monoid. *)

module Over (Tape : Preface_specs.MONOID) : sig
  include Preface_specs.TRACED with type tape = Tape.t

  val traced : (tape -> 'a) -> 'a t
  (** Promote a function from [tape] to ['a] into a trace. *)

  val run_identity : 'a t -> tape -> 'a
  (** Run the traced through the [identity].*)

  (** {2 Functor} *)

  module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t
end
