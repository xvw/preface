(** TODO *)

(** {1 Structure anatomy} *)

(** Standard requirement. *)
module type TYPE = sig
  type 'a f
end

module type CORE = sig
  include TYPE

  (** The type holded by [Freer]. *)
  type _ t =
    | Return : 'a -> 'a t
    | Bind : 'b f * ('b -> 'a t) -> 'a t

  val liftF : 'a f -> 'a t
  (** Lifting function from [g] to [t] *)
end

(** {1 API} *)

module type API = sig
  include CORE

  module Functor : Functor.API with type 'a t = 'a t

  module Applicative : Applicative.API with type 'a t = 'a t

  module Monad : Monad.API with type 'a t = 'a t

  include module type of Monad with type 'a t := 'a t
end

(** The complete interface of a [Freer_monad]. *)
