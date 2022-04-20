(** A [Freer monad] allows you to build a {!module:Preface_specs.Monad} from an
    arbitrary type (with one type parameter). It offers the same capabilities as
    a {!module:Preface_specs.Free_monad} but benefits from a lighter execution
    cost. *)

(** {1 Structure anatomy} *)

(** The natural transformation for [Freer Monad] to [Monad]. *)
module type TO_MONAD = sig
  type 'a t
  (** The type held by the [Freer monad]. *)

  type 'a f
  (** The parametric type (which, unlike a {!module:Preface_specs.Free_monad}
      don't need to be a {!module:Preface_specs.Functor}). *)

  type 'a monad
  (** The type held by the [Monad]. *)

  type ('a, 'b) handle = ('a -> 'b monad) -> 'a f -> 'b monad

  type 'a handler = { handler : 'b. ('b, 'a) handle }
  (** The handler type. Which is a [Natural transformation] from the
      [Freer Monad] to the given [Monad]. *)

  val run : 'a handler -> 'a t -> 'a monad
  (** Run the natural transformation over the [Free monad]. *)
end

(** The [Freer Monad] API without the {!module:Preface_specs.Monad} API. *)
module type CORE = sig
  type 'a f
  (** The parametric type (which, unlike a {!module:Preface_specs.Free_monad}
      don't need to be a {!module:Preface_specs.Functor}). *)

  (** The type held by [Freer monad]. *)
  type _ t =
    | Return : 'a -> 'a t
    | Bind : 'b f * ('b -> 'a t) -> 'a t

  type ('a, 'b) handle = ('a -> 'b) -> 'a f -> 'b

  type 'a handler = { handler : 'b. ('b, 'a) handle }
  (** The handler type. Which is a [Natural transformation] from the
      [Freer Monad] to an unwrapped [Identity monad]. *)

  val perform : 'a f -> 'a t
  (** Create a new ['a t] from a ['a f]. *)

  val run : 'a handler -> 'a t -> 'a
  (** Execute a given handler for given data *)

  module To_monad (Monad : Monad.CORE) :
    TO_MONAD
      with type 'a t := 'a t
       and type 'a f := 'a f
       and type 'a monad := 'a Monad.t
end

(** {1 Complete API} *)

(** The complete interface of a [Freer monad]. *)
module type API = sig
  include CORE
  (** @inline *)

  (** {1 Functor API}

      A [Freer monad] is also an {!module:Preface_specs.Functor}. *)

  module Functor : Functor.API with type 'a t = 'a t

  (** {1 Applicative API}

      A [Freer monad] is also an {!module:Preface_specs.Applicative}. *)

  module Applicative : Applicative.API with type 'a t = 'a t

  (** {1 Selective API}

      A [Freer monad] is also an {!module:Preface_specs.Selective}. *)

  module Selective : Selective.API with type 'a t = 'a t

  (** {1 Monad API}

      A [Freer monad] is also (obviously) a {!module:Preface_specs.Monad}. *)

  module Monad : Monad.API with type 'a t = 'a t

  include module type of Monad with type 'a t := 'a t
  (** @inline *)
end
