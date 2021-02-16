(** A [Freer_monad] allows you to build a monad from a given type. Such monad is
    equiped with two additional functions: one dedicated to the creation of a
    new data and another one for the interpretation. *)

(** {1 Structure anatomy} *)

(** Standard requirement. *)
module type CORE = sig
  type 'a f
  (** The parametric type. *)

  (** The type held by [Freer]. *)
  type _ t =
    | Return : 'a -> 'a t
    | Bind : 'b f * ('b -> 'a t) -> 'a t

  type 'a handler = { handler : 'b. ('b -> 'a) -> 'b f -> 'a }
  (** The handler type *)

  val perform : 'a f -> 'a t
  (** Create a new ['a t] from a ['a f]. *)

  val run : 'a handler -> 'a t -> 'a
  (** Execute a given handler for given data *)
end

(** {1 API} *)

(** The complete interface of a [Freer_monad]. *)
module type API = sig
  include CORE

  module Functor : Functor.API with type 'a t = 'a t

  module Applicative : Applicative.API with type 'a t = 'a t

  module Monad : Monad.API with type 'a t = 'a t

  include module type of Monad with type 'a t := 'a t
end
