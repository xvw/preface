(** A specialized version of a [State monad] with [Identity] as the inner monad.
    Since [Preface.Make.State] is a Transformer, this module exposes the
    classical State monad. *)

(** {1 Specialization}

    A [State Monad] is set up by a type. *)

module Over (State : Preface_specs.Types.T0) : sig
  include Preface_specs.STATE with type state = State.t
  (** @inline*)

  val run_identity : 'a t -> State.t -> 'a * State.t
  (** Run the state through the [identity].*)

  val exec_identity : 'a t -> State.t -> State.t
  (** Exec the state through the [identity].*)

  val eval_identity : 'a t -> State.t -> 'a
  (** Eval the state through the [identity].*)

  (** {2 Functor} *)

  module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t

  (** {2 Applicative} *)

  module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t

  (** {2 Invariant} *)

  module Invariant : Preface_specs.INVARIANT with type 'a t = 'a t
end
