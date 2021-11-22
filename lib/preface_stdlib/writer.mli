(** A specialized version of a [Writer monad] with [Identity] as the inner
    monad. Since [Preface.Make.Writer] is a Transformer, this module exposes the
    classical Writer monad. *)

(** {1 Specialization giving a Monoid}

    A [Writer Monad] is set up by the monoidal tape which be written. *)

module Over (Tape : Preface_specs.MONOID) : sig
  include Preface_specs.WRITER with type tape = Tape.t
  (** @inline *)

  val run_identity : 'a t -> 'a * Tape.t
  (** Run the writer through the [identity].*)

  val exec_identity : 'a t -> Tape.t
  (** Exec the writer through the [identity].*)

  (** {2 Functor} *)

  module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t

  (** {2 Applicative} *)

  module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t

  (** {2 Invariant} *)

  module Invariant : Preface_specs.INVARIANT with type 'a t = 'a t
end
