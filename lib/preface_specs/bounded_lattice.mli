(** A [Bounded_lattice] capture the notion of Bounded join semilattice and a
    Bounded meet semilattice *)

(** {1 Minimal definition}*)

module type WITH_BOUNDED_JOIN_AND_BOUNDED_MEET = sig
  include Bounded_join_semilattice.WITH_JOIN_AND_BOTTOM
  include Bounded_meet_semilattice.WITH_MEET_AND_TOP with type t := t
end

(** {1 Structure anatomy} *)

module type CORE = WITH_BOUNDED_JOIN_AND_BOUNDED_MEET
(** Basis operations *)

(** Infix operators. *)
module type INFIX = sig
  include Bounded_join_semilattice.INFIX
  (** @inline *)

  include Bounded_meet_semilattice.INFIX with type t := t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of a [Bounded_lattice]. *)
module type API = sig
  include Bounded_join_semilattice.API
  include Bounded_meet_semilattice.API with type t := t

  (** {1 Functions} *)
  include CORE with type t := t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type t = t

  include INFIX with type t := t
  (** @inline *)
end
