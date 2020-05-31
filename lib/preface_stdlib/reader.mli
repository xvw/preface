(** TODO *)

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
