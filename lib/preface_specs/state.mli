(** A [State monad] parametrized over an inner {!module:Monad} and a state
    (which is an arbitrary type). [State] is a {e monad transformer}.*)

(** Operation of [State] monad parametrized over an inner monad and [state]. *)
module type CORE = sig
  type state
  (** The encapsulated state. *)

  type 'a monad
  (** The inner monad.*)

  type 'a t = state -> ('a * state) monad
  (** The type held by the state monad.*)

  val eval : 'a t -> state -> 'a monad
  (** Unwrap the state computation and extract the current value.*)

  val exec : 'a t -> state -> state monad
  (** Unwrap the state computation and extract the current state.*)

  val run : 'a t -> state -> ('a * state) monad
  (** Unwrap the state computation.*)

  val state : (state -> 'a * state) -> 'a t
  (** Lift a function into a state. *)

  val get : state t
  (** Returns the current state. *)

  val set : state -> unit t
  (** Replace the state with the given one. *)

  val modify : (state -> state) -> unit t
  (** Modify the state applying the given function. *)

  val gets : (state -> 'a) -> 'a t
  (** Apply a function to the current state and return it. *)
end

(** {1 Complete API} *)

(** The complete interface of a [State] monad wich introduces the
    {!module:Monad} API into the [State] API. *)
module type API = sig
  include CORE
  (** @closed *)

  (** {1 Monad API} *)

  module Monad : Monad.API

  (** {2 Monad API inclusion} *)

  include module type of Monad with type 'a t := 'a t
  (** @closed *)
end
