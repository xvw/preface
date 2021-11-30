(** A [Traced comonad] parametrized over an inner {!module:Comonad} and a tape
    (which is an arbitrary type). [Traced] is a {e comonad transformer}.
    [Traced] is [Cowriter] (the dual of [Writer]).*)

(** Operation of [Traced] comonad parametrized over an inner comonad and [tape]. *)
module type CORE = sig
  type tape
  (** The parameter which is a monoid. *)

  type 'a comonad
  (** The inner comonad. *)

  type 'a t = (tape -> 'a) comonad
  (** The type held by the traced comonad.*)

  (** @inline *)
  include
    Comonad.TRANSFORMER with type 'a t := 'a t and type 'a comonad := 'a comonad

  val run : 'a t -> (tape -> 'a) comonad
  (** Unwrap the traced computation. *)

  val trace : tape -> 'a t -> 'a
  (** Extracts the value at the current position. *)

  val traces : ('a -> tape) -> 'a t -> 'a
  (** Extracts the value at a relative position. *)

  val listen : 'a t -> ('a * tape) t
  (** Get the current position. *)

  val listens : (tape -> 'b) -> 'a t -> ('a * 'b) t
  (** Get the current position relatively. *)

  val censor : (tape -> tape) -> 'a t -> 'a t
  (** Apply a function to the current position. *)
end

(** {1 Complete API} *)

(** The complete interface of a [Traced] comonad which introduces the
    {!module:Comonad} API into the [Traced] API. *)
module type API = sig
  include CORE
  (** @inline *)

  (** {2 Comonad} *)

  module Comonad : Comonad.API

  include module type of Comonad with type 'a t := 'a t
  (** @inline *)
end
