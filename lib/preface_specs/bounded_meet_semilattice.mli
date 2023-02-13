(** A [Bounded_meet_semilattice] capture the notion of a set with
    {b greatest lower bound} with a top. A bounded meet semilattice is an
    idempotent commutative semigroup.

    {2 Laws}

    To have a predictable behaviour, the instance of [Bounded_meet_semilattice]
    must obey some laws.

    - [meet a (meet b c) = meet (meet a b) c]
    - [meet a b = meet b a]
    - [meet a a = a]
    - [meet a top = a] *)

(** {1 Minimal definition}*)

module type WITH_TOP = sig
  type t
  (** The type held by the [Bounded_meet_semilattice] *)

  val top : t
  (** the top value represent the greatest element of the meet semilattice *)
end

module type WITH_MEET_AND_TOP = sig
  include WITH_TOP
  include Meet_semilattice.WITH_MEET with type t := t
end

(** {1 Structure anatomy} *)

module type CORE = WITH_MEET_AND_TOP
(** Basis operations. *)

(** Infix operators. *)
module type INFIX = sig
  type t
  (** The type held by the [Bounded_meet_semilattice] *)

  include Meet_semilattice.INFIX with type t := t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of a [Bounded_meet_semillatice]. *)
module type API = sig
  type t
  (** The type hold by the [Bounded_meet_semilattice] *)

  (** {1 Functions} *)
  include CORE with type t := t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type t := t

  include INFIX with type t := t
  (** @inline *)
end
