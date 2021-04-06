(** A [Free monad] allows you to build a {!module:Monad} from a given
    {!module:Functor}. *)

(**Such {!module:Monad} is equiped with two additional functions: one dedicated
   to the creation of a new data and another one for the effect handling. *)

(** {2 Note about complexity}

    Although free constructs are elegant, they introduce an execution cost due
    to the recursive nature of defining the type of a [Free Monad]. There are
    {e cheaper} encodings like {!module:Freer_monad}, which removes the
    requirement for the [Free monad] parameter to be a {!module:Functor} and
    therefore does not increase its complexity.*)

(** {1 Structure anatomy} *)

(** The [Free Monad] API without the {!module:Monad} API. *)
module type CORE = sig
  type 'a f
  (** The type held by the {!module:Functor}. *)

  (** The type held by the [Free monad]. *)
  type 'a t =
    | Return of 'a
    | Bind of 'a t f

  val perform : 'a f -> 'a t
  (** Create a new ['a t] from a ['a f]. *)

  val run : ('a f -> 'a) -> 'a t -> 'a
  (** Execute a given handler for given data *)
end

(** {1 Complete API} *)

(** The complete interface of a [Free monad]. *)
module type API = sig
  include CORE
  (** @inline *)

  (** {1 Functor API}

      A [Free monad] is also a {!module:Functor}. *)

  module Functor : Functor.API with type 'a t = 'a t

  (** {1 Applicative API}

      A [Free monad] is also an {!module:Applicative}. *)

  module Applicative : Applicative.API with type 'a t = 'a t

  (** {1 Monad API}

      A [Free monad] is also (obviously) a {!module:Monad}. *)

  module Monad : Monad.API with type 'a t = 'a t

  (** {2 Monad API inclusion} *)

  include module type of Monad with type 'a t := 'a t
  (** @closed *)
end
