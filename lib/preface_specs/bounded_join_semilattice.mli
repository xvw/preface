(** A [Join_semilattice] capture the notion of a set with
    {b least upper bound} with a { bottom }. A bounded join semilattice is an idempotent commutative
    semigroup.
    {2 Laws}
    To have a predictable behaviour, the instance of [Bounded_join_semilattice] must
    obey some laws.
    - [join a (join b c) = join (join a b) c]
    - [join a b = join b a]
    - [join a a = a] 
    - [join a bottom = a] *)

(** {1 Minimal definition}*)

module type WITH_BOTTOM = sig
  type t
  (** The type held by the [Bounded_join_semilattice] *)

  val bottom : t
  (** The bottom value represent the least element of the join semilattice *)
end

module type WITH_JOIN_AND_BOTTOM = sig
  include WITH_BOTTOM
  include Join_semilattice.WITH_JOIN with type t := t
end

(** {1 Structure anatomy} *)

module type CORE = WITH_JOIN_AND_BOTTOM
(** Basis operations *)

(** Infix operators. *)
module type INFIX = sig
  type t
  (** The type held by the [Bounded_join_semilattice] *)

  include Join_semilattice.INFIX with type t := t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of a [Bounded_join_semillatice]. *)
module type API = sig
  type t
  (** The type hold by the [Bounded_join_semilattice] *)

  (** {1 Functions} *)
  include CORE with type t := t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type t = t

  include INFIX with type t := t
  (** @inline *)
end
