(** A [Category] is a generalization of function composition to morphisms. To be
    honnest, [Category] is only implemented in order to try to have a nice
    [Arrow] interface.*)

(** {1 Structure anatomy} *)

(** Requirement via [id] and [compose]. *)
module type CORE = sig
  type ('a, 'b) t
  (** The type held by the [Category]. *)

  val id : ('a, 'a) t
  (** The identity morphism. *)

  val compose : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
  (** Morphism composition (from right to left). *)
end

module type OPERATION = sig
  type ('a, 'b) t
  (** The type held by the [Category]. *)

  val compose_right_to_left : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
  (** An alias of {!val:CORE.compose}. *)

  val compose_left_to_right : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  (** An alias of {!val:CORE.compose} with flipped argument. *)
end

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
  (** An alias of {!val:CORE.compose} (to be iso with Haskell's approach). *)

  val ( >>> ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  (** An alias of {!val:CORE.compose_left_to_right} (to be iso with Haskell's
      approach). *)
end

(** {1 API} *)

(** The complete interface of a [Category]. *)
module type API = sig
  include CORE

  include OPERATION with type ('a, 'b) t := ('a, 'b) t

  module Infix : INFIX with type ('a, 'b) t = ('a, 'b) t

  include INFIX with type ('a, 'b) t := ('a, 'b) t
end

(** {1 Bibliography}

    - {{:https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Category.html}
      Haskell's documentation of Category} *)
