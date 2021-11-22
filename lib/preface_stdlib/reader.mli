(** A specialized version of a [Reader monad] with [Identity] as the inner
    monad. Since [Preface.Make.Reader] is a Transformer, this module exposes the
    classical [Reader] monad. *)

(** {1 Specialization giving an Environment}

    A [Reader Monad] is set up by the environment which be requested. *)

module Over (Env : Preface_specs.Types.T0) : sig
  include Preface_specs.READER with type env = Env.t
  (** @inline*)

  val run_identity : 'a t -> Env.t -> 'a
  (** Run the reader through the [identity].*)

  (** {2 Functor} *)

  module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t

  (** {2 Invariant} *)

  module Invariant : Preface_specs.INVARIANT with type 'a t = 'a t

  (** {2 Applicative} *)

  module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t
end
