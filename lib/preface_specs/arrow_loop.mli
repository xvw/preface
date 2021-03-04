(** An [Arrow] with a loop capabilities. *)

(** {1 Structure anatomy} *)

(** Loop operation *)
module type WITH_LOOP = sig
  type ('a, 'b) t
  (** The type held by the [Arrow_loop]. *)

  val loop : ('a * 'c, 'b * 'c) t -> ('a, 'b) t
  (** The loop operator expresses computations in which an output value is fed
      back as input, although the computation occurs only once. *)
end

(** Requirement via [arrow] and [fst] and [loop]. *)
module type CORE_WITH_ARROW_AND_FST_AND_LOOP = sig
  include WITH_LOOP

  include Arrow.CORE_WITH_ARROW_AND_FST with type ('a, 'b) t := ('a, 'b) t
end

(** Requirement via [arrow] and [fst] and [loop]. *)
module type CORE_WITH_ARROW_AND_SPLIT_AND_LOOP = sig
  include WITH_LOOP

  include Arrow.CORE_WITH_ARROW_AND_SPLIT with type ('a, 'b) t := ('a, 'b) t
end

(** Standard requirement *)
module type CORE = sig
  include WITH_LOOP

  include Arrow.CORE with type ('a, 'b) t := ('a, 'b) t
end

module type OPERATION = Arrow.OPERATION
(** Operations. *)

module type ALIAS = Arrow.ALIAS
(** Aliases of operations functions. *)

module type INFIX = Arrow.INFIX
(** Infix operators. *)

(** {1 API} *)

(** The complete interface of an [Arrow_loop]. *)
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
