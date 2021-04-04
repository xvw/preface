(** A [Monoid] is a type [t] which provides a binary associative operation
    [combine] and a neutral element ([neutral]). In other words, a [Monoid] is a
    {!module:Semigroup} with a neutral element. *)

(** {2 Laws}

    To ensure that the derived combiners work properly, a functor should respect
    these laws:

    + [combine (combine a b) c = combine a (combine b c)] (from
      {!module:Semigroup})
    + [combine x neutral = combine neutral x = x] *)

(** {1 Structure anatomy} *)

(** A type [t] with a neutral element. This signature is mainly used to enrich a
    [Semigroup] with a neutral element. *)
module type NEUTRAL = sig
  type t
  (** A type [t] which is a [Monoid]. *)

  val neutral : t
  (** The neutral element of the [Monoid]. *)
end

(** The minimum definition of a [Monoid]. It is by using the combinators of this
    module that the other combinators will be derived. *)
module type CORE = sig
  include Semigroup.CORE
  (** @inline *)

  include NEUTRAL with type t := t
  (** @closed *)
end

(** Additional operations. *)
module type OPERATION = sig
  include Semigroup.OPERATION
  (** @inline *)

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
  (** {1 Core functions}

      Set of fundamental functions in the description of a [Monoid]. *)

  include CORE
  (** @closed *)

  (** {1 Additional functions}

      Additional functions, derived from fundamental functions. *)

  include OPERATION with type t := t
  (** @closed *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type t = t

  (** {2 Infix operators inclusion} *)

  include INFIX with type t := t
  (** @closed *)
end

(** {1 Additional references}

    - {{:http://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Monoid.html}
      Haskell's documentation of a Monoid} *)
