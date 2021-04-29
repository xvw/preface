(** A specialized version of an [Env comonad] with [Identity] as the inner
    comonad. Since [Preface.Make.Env] is a Transformer, this module exposes the
    classical Env comonad (or [Coreader]). *)

(** {1 Specialization}

    An [Env Comonad] is set up by a type. *)

module Over (Env : Preface_specs.Types.T0) : sig
  include Preface_specs.ENV with type env = Env.t
  (** @inline*)

  val env : env -> 'a -> 'a t
  (** Create [Env] using an environment and a value. *)

  val run_identity : 'a t -> env * 'a
  (** Run the env through the [identity]. *)

  (** {2 Functor} *)

  module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t
end
