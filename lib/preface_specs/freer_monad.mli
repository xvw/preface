(** A [Freer monad] allows you to build a {!module:Monad} from an arbitrary type
    (with one type parameter). It offers the same capabilities as a
    {!module:Free_monad} but benefits from a lighter execution cost. *)

(** {1 Structure anatomy} *)

(** The [Freer Monad] API without the {!module:Monad} API. *)
module type CORE = sig
  type 'a f
  (** The parametric type (which, unlike a {!module:Free_monad} don't need to be
      a {!module:Functor}). *)

  (** The type held by [Freer monad]. *)
  type _ t =
    | Return : 'a -> 'a t
    | Bind : 'b f * ('b -> 'a t) -> 'a t

  type 'a handler = { handler : 'b. ('b -> 'a) -> 'b f -> 'a }
  (** The handler type. Which is a [Natural transformation] from the
      [Freer Monad] to an unwrapped [Identity monad]. *)

  val perform : 'a f -> 'a t
  (** Create a new ['a t] from a ['a f]. *)

  val run : 'a handler -> 'a t -> 'a
  (** Execute a given handler for given data *)
end

(** {1 Complete API} *)

(** The complete interface of a [Freer monad]. *)
module type API = sig
  include CORE
  (** @inline *)

  (** {1 Functor API}

      A [Freer monad] is also an {!module:Functor}. *)

  module Functor : Functor.API with type 'a t = 'a t

  (** {1 Applicative API}

      A [Freer monad] is also an {!module:Applicative}. *)

  module Applicative : Applicative.API with type 'a t = 'a t

  (** {1 Monad API}

      A [Freer monad] is also (obviously) a {!module:Monad}. *)

  module Monad : Monad.API with type 'a t = 'a t

  (** {2 Monad API inclusion} *)

  include module type of Monad with type 'a t := 'a t
  (** @closed *)
end
