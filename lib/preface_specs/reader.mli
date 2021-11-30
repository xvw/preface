(** A [Reader monad] parametrized over an inner {!module:Monad} and an
    environment. [Reader] is a {e monad transformer}.*)

(** {1 Structure anatomy} *)

(** Operation of [Reader] monad parametrized over an inner monad and an
    environment. *)
module type CORE = sig
  type env
  (** The encapsulated state. *)

  type 'a monad
  (** The inner monad.*)

  type 'a t = env -> 'a monad
  (** The type held by the reader monad.*)

  (** @inline *)
  include Monad.TRANSFORMER with type 'a t := 'a t and type 'a monad := 'a monad

  val run : 'a t -> env -> 'a monad
  (** Run the reader and extract the value. *)

  val ask : env t
  (** Provides the monad environment. *)

  val local : (env -> env) -> 'a t -> 'a t
  (** Modify the environment and execute the reader. *)

  val reader : (env -> 'a monad) -> 'a t
  (** Build a reader from a function. *)
end

(** {1 Complete API} *)

(** The complete interface of a [Reader] monad which introduces the
    {!module:Monad} API into the [Reader] API. *)
module type API = sig
  include CORE
  (** @inline *)

  (** {1 Monad} *)

  module Monad : Monad.API

  include module type of Monad with type 'a t := 'a t
  (** @inline *)
end
