(** A [Writer monad] parametrized over an inner {!module:Monad} and an tape
    which is a {!module:Monoid}. [Writer] is a {e monad transformer}.*)

(** {1 Structure anatomy} *)

(** Operation of [Writer] monad parametrized over an inner monad and a monoidal
    tape. *)
module type CORE = sig
  type tape
  (** The encapsulated tape. *)

  type 'a monad
  (** The inner monad.*)

  type 'a t = ('a * tape) monad
  (** The type held by the writer monad.*)

  val writer : 'a * tape -> 'a t
  (** Construct a writer computation from a (result, output) pair. *)

  val run : 'a t -> ('a * tape) monad
  (** Unwrap the writer computation.*)

  val exec : 'a t -> tape monad
  (** Extract the output from the writer computation. *)

  val tell : tape -> unit t
  (** [tell] helps to enrich the output. This is done thanks to the semigroup
      combine operation. *)

  val listen : 'a t -> ('a * tape) t
  (** [listen] executes the effect and return both the result and the
      corresponding output within the effect. *)

  val listens : (tape -> 'b) -> 'a t -> ('a * 'b) t
  (** Performs the action and adds the result of applying the function to the
      output to the value of the computation. *)

  val pass : ('a * (tape -> tape)) t -> 'a t
  (** [pass] executes the effect and apply the function to the corresponding
      output. *)

  val censor : (tape -> tape) -> 'a t -> 'a t
  (** [censor] executes the effects, apply the function to the corresponding
      output and returns an effects with the value unchanged. *)
end

(** {1 Complete API} *)

(** The complete interface of a [Writer] monad wich introduces the
    {!module:Monad} API into the [Writer] API. *)
module type API = sig
  include CORE
  (** @closed *)

  (** {1 Monad API} *)

  module Monad : Monad.API

  (** {2 Monad API inclusion} *)

  include module type of Monad with type 'a t := 'a t
  (** @closed *)
end
