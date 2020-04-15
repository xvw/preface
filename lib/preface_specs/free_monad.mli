(** A [Free_monad] allows you to build a monad from a given functor. *)

(** {1 Structure anatomy} *)

module type CORE = sig
  type 'a f

  type 'a t =
    | Return of 'a
    | Bind of 'a t f

  val liftF : 'a f -> 'a t

  val run : ('a f -> 'a) -> 'a t -> 'a
end

(** {1 API} *)

module type API = sig
  include CORE

  module Functor : Functor.API with type 'a t = 'a t

  module Applicative : Applicative.API with type 'a t = 'a t

  module Monad : Monad.API with type 'a t = 'a t

  include module type of Monad with type 'a t := 'a t
end

(** The complete interface of a [Free_monad]. *)
