(** A specialized version of a [Writer monad] with [Identity] as the inner
    monad. Since [Preface.Make.Writer] is a Transformer, this module exposes the
    classical [Writer] monad. *)

(** {1 Capabilities}

    - {!val:Functor}
    - {!val:Applicative}
    - {!val:Monad}

    {1 Use cases}

    The Writer module gives you the ability to accumulate values thanks to the
    parametric `Monoid`. Then for instance it can be used to create a log
    reflecting performed operations. *)

(** {1 Implementation} *)

module Over (Tape : Preface_specs.MONOID) : sig
  include Preface_specs.WRITER with type tape = Tape.t
  (** {2 Monad API} *)

  val run_identity : 'a t -> 'a * Tape.t
  (** Run the writer through the [identity].*)

  val exec_identity : 'a t -> Tape.t
  (** Exec the writer through the [identity].*)

  module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t
  (** {2 Functor API} *)

  module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t
  (** {2 Applicative API} *)
end
