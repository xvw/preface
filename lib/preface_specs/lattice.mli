(** A [Lattice] capture the notion of a join semilattice and a meet semilattice *)

(** {1 Minimal definition}*)

module type WITH_JOIN_AND_MEET = sig
  include Join_semilattice.WITH_JOIN
  include Meet_semilattice.WITH_MEET with type t := t
end

(** {1 Structure anatomy} *)

module type CORE = WITH_JOIN_AND_MEET
(** Basis operations *)

(** Infix operators. *)
module type INFIX = sig
  include Join_semilattice.INFIX
  (** @inline *)

  include Meet_semilattice.INFIX with type t := t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of a [Lattice]. *)
module type API = sig
  include Join_semilattice.API
  include Meet_semilattice.API with type t := t

  (** {1 Functions} *)
  include CORE with type t := t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type t := t

  include INFIX with type t := t
  (** @inline *)
end
