(** A [Semigroup] is a type [t] which provides a binary associative operation
    [combine] which lets you combine any two values of [t] into one. *)

(** {2 Laws}

    To ensure that the derived combiners work properly, the [combine] function
    must comply with the following laws:

    + [combine (combine a b) c = combine a (combine b c) ] *)

(** {1 Minimal definition} *)

(** The minimum definition of a [Semigroup]. It is by using the combinators of
    this module that the other combinators will be derived. *)
module type WITH_COMBINE = sig
  type t
  (** the type held by the [Semigroup]. *)

  val combine : t -> t -> t
  (** [combine x y] Combine two values ([x] and [y]) of [t] into one. *)
end

(** {1 Structure anatomy} *)

module type CORE = WITH_COMBINE
(** Basis operations.*)

(** Additional operations. *)
module type OPERATION = sig
  type t
  (** the type held by the [Semigroup]. *)

  val times_nel : int -> t -> t option
  (** [times_nel n x] apply [combine] on [x] [n] times. If [n] is lower than [1]
      the function will returns [None]. *)

  val reduce_nel : t Preface_core.Nonempty_list.t -> t
  (** Reduce a [Nonempty_list.t] using [combine]. *)
end

(** Infix operators. *)
module type INFIX = sig
  type t
  (** the type held by the [Semigroup]. *)

  val ( <|> ) : t -> t -> t
  (** Infix version of {!val:CORE.combine} *)
end

(** {1 Complete API} *)

(** The complete interface of a [Semigroup]. *)
module type API = sig
  type t
  (** the type held by the [Semigroup]. *)

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

    - {{:http://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Semigroup.html}
      Haskell's documentation of a Semigroup} *)
