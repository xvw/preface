(** A [Meet_semilattice] capture the notion of a set with
    {b greatest lower bound}. A meet semilattice is an idempotent commutative
    semigroup.

    {2 Laws}

    To have a predictable behaviour, the instance of [Meet_semilattice] must
    obey some laws.

    - [meet a (meet b c) = meet (meet a b) c]
    - [meet a b = meet b a]
    - [meet a a = a] *)

(** {1 Minimal definition} *)

module type WITH_MEET = sig
  type t
  (** The type held by the [Meet_semilattice]. *)

  val meet : t -> t -> t
  (** [meet x y] is the greatest lower bound of [{x, y}]. *)
end

(** {1 Structure anatomy} *)

module type CORE = WITH_MEET
(** Basis operations.*)

(** Infix operators. *)
module type INFIX = sig
  type t
  (** The type held by the [Meet_semilattice]. *)

  val ( && ) : t -> t -> t
  (** Infix version of [meet]. *)
end

(** {1 Complete API} *)

(** The complete interface of a [Meet_semilattice]. *)
module type API = sig
  type t
  (** The type held by the [Meet_semilattice]. *)

  (** {1 Functions} *)

  include CORE with type t := t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type t := t

  include INFIX with type t := t
  (** @inline *)
end
