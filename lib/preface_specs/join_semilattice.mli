(** A [Join_semilattice] capture the notion of a set with {b least upper bound}.
    A join semilattice is an idempotent commutative semigroup.

    {2 Laws}

    To have a predictable behaviour, the instance of [Join_semilattice] must
    obey some laws.

    - [join a (join b c) = join (join a b) c]
    - [join a b = join b a]
    - [join a a = a] *)

(** {1 Minimal definition}*)

module type WITH_JOIN = sig
  type t
  (** The type held by the [Join_semilattice]. *)

  val join : t -> t -> t
  (** [join x y] is the least upper bound of [{x,y}]. *)
end

(** {1 Structure anatomy} *)

module type CORE = WITH_JOIN
(** Basis operations. *)

(** Infix operators. *)
module type INFIX = sig
  type t
  (** The type held by the [Join_semillatice]. *)

  val ( || ) : t -> t -> t
  (** Infix version of [join]. *)
end

(** {1 Complete API} *)

(** The complete interface of a [Join_semillatice]. *)
module type API = sig
  type t
  (** The type held by the [Join_semillatice]. *)

  (** {1 Functions} *)
  include CORE with type t := t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type t = t

  include INFIX with type t := t
  (** @inline *)
end
