(** An [Arrow] with a combine and neutral. *)

(** {1 Structure anatomy} *)

(** Combine and neutral operation. *)
module type COMBINE_AND_NEUTRAL = sig
  include Arrow_zero.NEUTRAL

  include Arrow_alt.COMBINE with type ('a, 'b) t := ('a, 'b) t
end

(** Requirement via [fst], [neutral] and [combine]. *)
module type CORE_WITH_ARROW_AND_FST = sig
  include COMBINE_AND_NEUTRAL

  include Arrow.CORE_WITH_ARROW_AND_FST with type ('a, 'b) t := ('a, 'b) t
end

(** Requirement via [split], [neutral] and [combine]. *)
module type CORE_WITH_ARROW_AND_SPLIT = sig
  include COMBINE_AND_NEUTRAL

  include Arrow.CORE_WITH_ARROW_AND_SPLIT with type ('a, 'b) t := ('a, 'b) t
end

(** Standard requirement *)
module type CORE = sig
  include COMBINE_AND_NEUTRAL

  include Arrow.CORE with type ('a, 'b) t := ('a, 'b) t
end

(** Operations. *)
module type OPERATION = sig
  include Arrow_alt.OPERATION

  val reduce : ('a, 'b) t list -> ('a, 'b) t
  (** Reduce a [List.t] using [combine]. *)
end

module type ALIAS = Arrow.ALIAS
(** Aliases of operations functions. *)

(** Infix operators. *)
module type INFIX = sig
  include Arrow.INFIX

  val ( <|> ) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  (** Infix version of {!val:CORE.combine} *)
end

(** {1 API} *)

(** The complete interface of an [Arrow_plus]. *)
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
