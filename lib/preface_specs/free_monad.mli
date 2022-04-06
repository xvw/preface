(** A [Free monad] allows you to build a {!module:Preface_specs.Monad} from a
    given {!module:Preface_specs.Functor}. *)

(**Such {!module:Preface_specs.Monad} is equipped with two additional functions:
   one dedicated to the creation of a new data and another one for the effect
   handling. *)

(** {2 Note about complexity}

    Although free constructs are elegant, they introduce an execution cost due
    to the recursive nature of defining the type of a [Free Monad]. There are
    {e cheaper} encodings like {!module:Preface_specs.Freer_monad}, which
    removes the requirement for the [Free monad] parameter to be a
    {!module:Preface_specs.Functor} and therefore does not increase its
    complexity.*)

(** {1 Structure anatomy} *)

(** The natural transformation for [Free Monad] to [Monad]. *)
module type TO_MONAD = sig
  type 'a t
  (** The type held by the [Free monad]. *)

  type 'a f
  (** The type held by the {!module:Preface_specs.Functor}. *)

  type 'a monad
  (** The type held by the [Monad]. *)

  type natural_transformation = { transform : 'a. 'a f -> 'a monad }

  val run : natural_transformation -> 'a t -> 'a monad
  (** Run the natural transformation over the [Free monad]. *)
end

(** The [Free Monad] API without the {!module:Preface_specs.Monad} API. *)
module type CORE = sig
  type 'a f
  (** The type held by the {!module:Preface_specs.Functor}. *)

  (** The type held by the [Free monad]. *)
  type 'a t =
    | Return of 'a
    | Bind of 'a t f

  val perform : 'a f -> 'a t
  (** Create a new ['a t] from a ['a f]. *)

  val run : ('a f -> 'a) -> 'a t -> 'a
  (** Execute a given handler for given data *)

  module To_monad (Monad : Monad.CORE) :
    TO_MONAD
      with type 'a t := 'a t
       and type 'a f := 'a f
       and type 'a monad := 'a Monad.t
end

(** {1 Complete API} *)

(** The complete interface of a [Free monad]. *)
module type API = sig
  include CORE
  (** @inline *)

  (** {1 Functor API}

      A [Free monad] is also a {!module:Preface_specs.Functor}. *)

  module Functor : Functor.API with type 'a t = 'a t

  (** {1 Applicative API}

      A [Free monad] is also an {!module:Preface_specs.Applicative}. *)

  module Applicative : Applicative.API with type 'a t = 'a t

  (** {1 Selective API}

      A [Free monad] is also an {!module:Preface_specs.Selective}. *)

  module Selective : Selective.API with type 'a t = 'a t

  (** {1 Monad API}

      A [Free monad] is also (obviously) a {!module:Preface_specs.Monad}. *)

  module Monad : Monad.API with type 'a t = 'a t

  include module type of Monad with type 'a t := 'a t
  (** @inline *)
end
