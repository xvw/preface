(** A [Monoid] is a type [t] which provides a binary associative operation
    [combine] and a neutral element ([neutral]). In other words, a [Monoid] is a
    {!module:Semigroup} with a neutral element. *)

(** {2 Laws}

    To ensure that the derived combiners work properly, a functor should respect
    these laws:

    + [combine (combine a b) c = combine a (combine b c)] (from
      {!module:Semigroup})
    + [combine x neutral = combine neutral x = x] *)

(** {1 Minimal definition} *)

(** A type [t] with a neutral element. This signature is mainly used to enrich a
    [Semigroup] with a neutral element. *)
module type WITH_NEUTRAL = sig
  type t
  (** the type held by the [Monoid]. *)

  val neutral : t
  (** The neutral element of the [Monoid]. *)
end

module type WITH_NEUTRAL_AND_COMBINE = sig
  type t
  (** the type held by the [Monoid]. *)

  include Semigroup.CORE with type t := t
  (** @inline *)

  include WITH_NEUTRAL with type t := t
  (** @inline *)
end

(** {1 Structure anatomy} *)

module type CORE = WITH_NEUTRAL_AND_COMBINE
(** Basis operations.*)

(** Additional operations. *)
module type OPERATION = sig
  include Semigroup.OPERATION
  (** @inline *)

  val times : int -> t -> t
  (** [times n x] apply [combine] on [x] [n] times. If [n] is lower than [1] the
      function will returns [neutral]. *)

  val reduce : t list -> t
  (** Reduce a [List.t] using [combine]. *)
end

(** Infix operators. *)
module type INFIX = sig
  include Semigroup.INFIX
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of a [Monoid]. *)
module type API = sig
  (** {1 Type} *)

  type t
  (** the type held by the [Monoid]. *)

  (** {1 Functions} *)

  include CORE with type t := t
  (** @inline *)

  include OPERATION with type t := t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type t := t

  include INFIX with type t := t
  (** @inline *)
end

(** {1 Additional references}

    - {{:http://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Monoid.html}
      Haskell's documentation of a Monoid} *)
