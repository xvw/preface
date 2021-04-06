(** A [Category] is a generalization of function composition to morphisms. *)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Category] must obey some
    laws.

    + [f % id = f]
    + [id % f = f]
    + [f % (g % h) = (f % g) % h] *)

(** {1 Minimal definition} *)

(** Minimal interface using [id] and [compose]. *)
module type WITH_ID_AND_COMPOSE = sig
  type ('a, 'b) t
  (** The type held by the [Category]. *)

  val id : ('a, 'a) t
  (** The identity morphism. *)

  val compose : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
  (** Morphism composition (from right to left). *)
end

(** {1 Structure anatomy} *)

module type CORE = WITH_ID_AND_COMPOSE
(** Basis operations. *)

(** Additional operations. *)
module type OPERATION = sig
  type ('a, 'b) t
  (** The type held by the [Category]. *)

  val compose_right_to_left : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
  (** An alias of {!val:CORE.compose}. *)

  val compose_left_to_right : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  (** An alias of {!val:CORE.compose} with flipped argument. *)
end

(** Infix operators. *)
module type INFIX = sig
  type ('a, 'b) t
  (** The type held by the [Category]. *)

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

(** The complete interface of a [Category]. *)
module type API = sig
  (** {1 Core functions}

      Set of fundamental functions in the description of a [Category]. *)

  include CORE
  (** @closed *)

  (** {1 Additional functions}

      Additional functions, derived from fundamental functions. *)

  include OPERATION with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type ('a, 'b) t = ('a, 'b) t

  (** {2 Infix operators inclusion} *)

  include INFIX with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** {1 Additional references}

    - {{:https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Category.html}
      Haskell's documentation of Category} *)
