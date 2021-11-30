(** A [Semigroupoid] is [Category] without the identity. *)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Semigroupoid] must obey
    one law.

    + [f % (g % h) = (f % g) % h] *)

(** {1 Minimal definition} *)

(** Minimal interface using [id] and [compose]. *)
module type WITH_COMPOSE = sig
  type ('a, 'b) t
  (** The type held by the [Category]. *)

  val compose : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
  (** Morphism composition (from right to left). *)
end

(** {1 Structure anatomy} *)

module type CORE = WITH_COMPOSE
(** Basis operations. *)

(** Additional operations. *)
module type OPERATION = sig
  type ('a, 'b) t
  (** The type held by the [Semigroupoid]. *)

  val compose_right_to_left : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
  (** An alias of {!val:CORE.compose}. *)

  val compose_left_to_right : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  (** An alias of {!val:CORE.compose} with flipped argument. *)
end

(** Infix operators. *)
module type INFIX = sig
  type ('a, 'b) t
  (** The type held by the [Semigroupoid]. *)

  val ( % ) : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
  (** An alias of {!val:CORE.compose} (to be iso with [Preface_core]). *)

  val ( <% ) : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
  (** An alias of {!val:CORE.compose} (to be iso with [Preface_core]). *)

  val ( %> ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  (** An alias of {!val:OPERATION.compose_left_to_right} (to be iso with
      [Preface_core]). *)

  val ( <<< ) : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
  (** An alias of {!val:CORE.compose} (to be iso with Haskell's approach). Even
      [<<<] looks like [<%] (it is an alias for the same function), they differ
      in their priorities.
      {{:https://ocaml.org/manual/expr.html#ss%3Aprecedence-and-associativity}
      OCaml documentation of operators priorities} *)

  val ( >>> ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  (** An alias of {!val:CORE.compose_left_to_right} (to be iso with Haskell's
      approach). Even [>>>] looks like [%>] (it is an alias for the same
      function), they differ in their priorities.
      {{:https://ocaml.org/manual/expr.html#ss%3Aprecedence-and-associativity}
      OCaml documentation of operators priorities} *)
end

(** {1 Complete API} *)

(** The complete interface of a [Semigroupoid]. *)
module type API = sig
  (** {1 Type} *)

  type ('a, 'b) t
  (** The type held by the [Semigroupoid]. *)

  (** {1 Functions} *)

  include CORE with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)

  include OPERATION with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type ('a, 'b) t = ('a, 'b) t

  include INFIX with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** {1 Additional references}

    - {{:https://hackage.haskell.org/package/semigroupoids} Haskell's
      documentation of Semigroupoid} *)
