(** An [Arrow] with a neutral element. *)

(** {1 Structure anatomy} *)

(** Zero operation. *)
module type NEUTRAL = sig
  type ('a, 'b) t
  (** The type held by the [Arrow_zero]. *)

  val neutral : ('a, 'b) t
  (** The neutral element of the [Arrow_zero]. *)
end

(** Requirement via [arrow] and [neutral]. *)
module type CORE_WITH_ARROW = sig
  include NEUTRAL

  include Arrow.CORE_WITH_ARROW with type ('a, 'b) t := ('a, 'b) t
end

(** Requirement via [fst] and [neutral]. *)
module type CORE_WITH_ARROW_AND_FST = sig
  include NEUTRAL

  include Arrow.CORE_WITH_ARROW_AND_FST with type ('a, 'b) t := ('a, 'b) t
end

(** Requirement via [split] and [neutral]. *)
module type CORE_WITH_ARROW_AND_SPLIT = sig
  include NEUTRAL

  include Arrow.CORE_WITH_ARROW_AND_SPLIT with type ('a, 'b) t := ('a, 'b) t
end

(** Standard requirement *)
module type CORE = sig
  include NEUTRAL

  include Arrow.CORE with type ('a, 'b) t := ('a, 'b) t
end

module type OPERATION = Arrow.OPERATION
(** Operations. *)

module type ALIAS = Arrow.ALIAS
(** Aliases of operations functions. *)

module type INFIX = Arrow.INFIX
(** Infix operators. *)

(** {1 API} *)

(** The complete interface of an [Arro_zero]. *)
module type API = sig
  include CORE

  include OPERATION with type ('a, 'b) t := ('a, 'b) t

  include ALIAS with type ('a, 'b) t := ('a, 'b) t

  module Infix : INFIX with type ('a, 'b) t = ('a, 'b) t

  include INFIX with type ('a, 'b) t := ('a, 'b) t
end

(** {1 Bibliography}

    - {{:https://www.haskell.org/arrows/} Arrows: A General Interface to
      Computation}
    - {{:https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Arrow.html}
      Haskell's documentation of Arrow} *)
