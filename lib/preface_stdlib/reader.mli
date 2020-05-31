(** {1 Capabilities}

    - {!val:Functor}
    - {!val:Applicative}
    - {!val:Monad}

    {1 Use cases}

    The Reader module allows you to deal with the capability to read from an
    environment. This is done thanks to the `run` function that takes an
    environment and returns a value. Such environment type is given by applying
    the `Over` functor module on a module providing the required type.

    {1 Example}

    TODO *)

(** {1 Implementation} *)
module Over (T : Preface_specs.Types.T0) : sig
  (** {2 Types} *)

  type env = T.t
  (** The encapsulated state *)

  type 'a t
  (** The type *)

  val run : 'a t -> env -> 'a
  (** Run the reader and extracting the value *)

  module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t
  (** {2 Functor API} *)

  module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t
  (** {2 Applicative API} *)

  module Monad : Preface_specs.MONAD with type 'a t = 'a t
  (** {2 Monad API} *)
end
