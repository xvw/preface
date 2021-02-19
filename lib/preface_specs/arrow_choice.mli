(** An [Arrow] with a conditionals capabilities. *)

(** {1 Structure anatomy} *)

(** Left operation. *)
module type WITH_LEFT = sig
  type ('a, 'b) t
  (** The type held by the [Arrow_choice]. *)

  val left : ('a, 'b) t -> (('a, 'c) Either.t, ('b, 'c) Either.t) t
  (** Feed marked inputs through the argument arrow, passing the rest through
      unchanged to the output. *)
end

(** Choose operation. *)
module type WITH_CHOOSE = sig
  type ('a, 'b) t
  (** The type held by the [Arrow_choice]. *)

  val choose :
    ('a, 'b) t -> ('c, 'd) t -> (('a, 'c) Either.t, ('b, 'd) Either.t) t
  (** Split the input between the two argument arrows, retagging and merging
      their outputs. *)
end

(** Choose and left operation. *)
module type WITH_LEFT_AND_CHOOSE = sig
  include WITH_LEFT

  include WITH_CHOOSE with type ('a, 'b) t := ('a, 'b) t
end

(** Requirement via [arrow] and [fst] and [choose]. *)
module type CORE_WITH_ARROW_AND_FST_AND_CHOOSE = sig
  include WITH_CHOOSE

  include Arrow.CORE_WITH_ARROW_AND_FST with type ('a, 'b) t := ('a, 'b) t
end

(** Requirement via [arrow] and [fst] and [left]. *)
module type CORE_WITH_ARROW_AND_FST_AND_LEFT = sig
  include WITH_LEFT

  include Arrow.CORE_WITH_ARROW_AND_FST with type ('a, 'b) t := ('a, 'b) t
end

(** Requirement via [arrow] and [fst] and [choose]. *)
module type CORE_WITH_ARROW_AND_SPLIT_AND_CHOOSE = sig
  include WITH_CHOOSE

  include Arrow.CORE_WITH_ARROW_AND_SPLIT with type ('a, 'b) t := ('a, 'b) t
end

(** Requirement via [arrow] and [fst] and [left]. *)
module type CORE_WITH_ARROW_AND_SPLIT_AND_LEFT = sig
  include WITH_LEFT

  include Arrow.CORE_WITH_ARROW_AND_SPLIT with type ('a, 'b) t := ('a, 'b) t
end

(** Standard requirement *)
module type CORE = sig
  include WITH_LEFT

  include WITH_CHOOSE with type ('a, 'b) t := ('a, 'b) t

  include Arrow.CORE with type ('a, 'b) t := ('a, 'b) t
end

(** Operations. *)
module type OPERATION = sig
  include Arrow.OPERATION

  val right : ('a, 'b) t -> (('c, 'a) Either.t, ('c, 'b) Either.t) t
  (** The mirror image of [left]. *)

  val fan_in : ('a, 'c) t -> ('b, 'c) t -> (('a, 'b) Either.t, 'c) t
  (** Split the input between the two argument arrows and merge their outputs. *)
end

module type ALIAS = Arrow.ALIAS
(** Aliases of operations functions. *)

(** Infix operators. *)
module type INFIX = sig
  include Arrow.INFIX

  val ( +++ ) :
    ('a, 'b) t -> ('c, 'd) t -> (('a, 'c) Either.t, ('b, 'd) Either.t) t
  (** Infix version of {!val:CORE.choose}. *)

  val ( ||| ) : ('a, 'c) t -> ('b, 'c) t -> (('a, 'b) Either.t, 'c) t
  (** Infix version of {!val:CORE.fan_in}. *)
end

(** {1 API} *)

(** The complete interface of an [Arrow_alt]. *)
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
