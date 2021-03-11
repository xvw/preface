(** {1 Structure anatomy} *)
module type CORE = sig
  (** {2 Types} *)

  type env
  (** The encapsulated state *)

  type 'a t
  (** The type *)

  module Functor : Functor.API with type 'a t = 'a t
  (** {2 Functor API} *)

  module Applicative : Applicative.API with type 'a t = 'a t
  (** {2 Applicative API} *)

  module Monad : Monad.API with type 'a t = 'a t
  (** {2 Monad API} *)

  (** {2 Helpers} *)

  val run : 'a t -> env -> 'a
  (** Run the reader and extracting the value *)

  val ask : env t
  (** Provides the monad environment *)

  val local : (env -> env) -> 'a t -> 'a t
  (** Modify the environment and execute the reader *)

  val reader : (env -> 'a) -> 'a t
  (** Build a reader from a function *)
end

module type API = sig
  include CORE
end
