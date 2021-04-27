(** An [Env comonad] parametrized over an inner {!module:Comonad} and an env
    (which is an arbitrary type). [Env] is a {e comonad transformer}. [Env] is
    [Coreader] (the dual of [Reader]).*)

(** Operation of [Env] comonad parametrized over an inner comonad and [env]. *)
module type CORE = sig
  type env
  (** The encapsulated environment. *)

  type 'a comonad
  (** The inner comonad. *)

  type 'a t = env * 'a comonad
  (** The type held by the env comonad.*)

  val run : 'a t -> env * 'a comonad
  (** Unwrap the env computation. *)

  val ask : 'a t -> env
  (** Retreives the environment. *)

  val asks : (env -> 'b) -> 'a t -> 'b
  (** Retreives the environment and apply a function. *)

  val local : (env -> env) -> 'a t -> 'a t
  (** Modifies the environment using the specified function. *)

  (** Perform [local] with an other environment.*)
  module Local (Env : Types.T0) : sig
    type 'a out = Env.t * 'a comonad
    (** [Env] comonad with an other environment*)

    val run : (env -> Env.t) -> 'a t -> 'a out
    (** Modifies the environment using the specified function. *)
  end
end

(** {1 Complete API} *)

(** The complete interface of a [Env] comonad wich introduces the
    {!module:Comonad} API into the [Env] API. *)
module type API = sig
  include CORE
  (** @inline *)

  (** {2 Comonad} *)

  module Comonad : Comonad.API

  include module type of Comonad with type 'a t := 'a t
  (** @inline *)
end
