(** A [Monoid] is a type [t] which provides an associative operation [concat]
    (so a [Monoid] is also a [Semigroup]) and a neutral element ([zero]).

    {1 Structure anatomy} *)

(** A type [t] with a neutral element. *)
module type NEUTRAL = sig
  type t
  (** A type [t] which is a [Monoid]. *)

  val neutral : t
  (** The neutral element of the [monoid]. *)
end

(** Standard requirement *)
module type CORE = sig
  include Semigroup.CORE

  include NEUTRAL with type t := t
end

(** Operations *)
module type OPERATION = sig
  include Semigroup.OPERATION

  val reduce : t list -> t
  (** Reduce a [List.t] using [combine]. *)
end

module type INFIX = Semigroup.INFIX
(** Infix notation *)

(** The complete interface of a [Monoid]. *)
module type API = sig
  include CORE

  include OPERATION with type t := t

  module Infix : INFIX with type t = t

  include INFIX with type t := t
end

(** {1 Bibliography}

    - {{:http://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Monoid.html}
      Haskell's documentation of a Monoid} *)
