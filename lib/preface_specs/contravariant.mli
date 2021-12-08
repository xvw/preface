(** [Contravariant] is a "Contravariant functor". In other word, [Contravariant]
    is the dual of a {!module:Functor}.*)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Contravariant] must obey
    some laws.

    + [contramap id = id]
    + [(contramap f) % (contramap g) = contramap (g % f)] *)

(** {1 Minimal definition} *)

(** The minimum definition of a [Contravariant]. It is by using the combinators
    of this module that the other combinators will be derived. *)
module type WITH_CONTRAMAP = sig
  type 'a t
  (** The type held by the [Contravariant Functor]. *)

  val contramap : ('a -> 'b) -> 'b t -> 'a t
  (** Mapping over from ['a] to ['b] over ['b t] to ['a t]. *)
end

(** {1 Structure anatomy} *)

module type CORE = WITH_CONTRAMAP
(** Basis operations.*)

(** Additional operations. *)
module type OPERATION = sig
  type 'a t
  (** The type held by the [Contravariant Functor]. *)

  val replace : 'b -> 'b t -> 'a t
  (** Replace all locations in the output with the same value. *)
end

(** Infix operators. *)
module type INFIX = sig
  type 'a t
  (** The type held by the [Contravariant Functor]. *)

  val ( >$ ) : 'b -> 'b t -> 'a t
  (** Infix version of {!val:OPERATION.replace}. *)

  val ( $< ) : 'b t -> 'b -> 'a t
  (** Infix flipped version of {!val:OPERATION.replace}. *)

  val ( >$< ) : ('a -> 'b) -> 'b t -> 'a t
  (** Infix version of {!val:CORE.map}. *)

  val ( >&< ) : 'b t -> ('a -> 'b) -> 'a t
  (** Infix flipped version of {!val:CORE.map}. *)
end

(** {1 Complete API} *)

(** The complete interface of a [Contravariant Functor]. *)
module type API = sig
  (** {1 Type} *)
  type 'a t
  (** The type held by the [Contravariant Functor]. *)

  (** {1 Functions} *)

  include CORE with type 'a t := 'a t
  (** @inline *)

  include OPERATION with type 'a t := 'a t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type 'a t = 'a t

  include INFIX with type 'a t := 'a t
  (** @inline *)
end

(** {1 Additional references}

    - {{:https://typeclasses.com/contravariance}
      https://typeclasses.com/contravariance} *)
