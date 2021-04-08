(** An [Arrow_choice] is an {!module:Arrow} with conditional capabilities. It is
    a kind of {!module:Selective} in the arrow hierarchy. And an [Arrow_choice]
    is also {!module:Arrow}. *)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Arrow_choice] must obey
    some laws.

    + All {!module:Arrow} laws
    + [left (arrow f) = arrow (f ++ id)]
    + [left (f >>> g) = left f >>> left g]
    + [f >>> arr Left = arr Left >>> left f]
    + [left f >>> arrow (Fun.id +++ g) = arrow (Fun.id +++ g) >>> left f]
    + [left (left f) >>> arrow assocsum = arrow assocsum >>> left f] *)

open Preface_core.Shims

(** {1 Minimal definition} *)

(** Minimal definition using [left] operation without {!module:Arrow}. *)
module type WITH_LEFT = sig
  type ('a, 'b) t
  (** The type held by the [Arrow_choice]. *)

  val left : ('a, 'b) t -> (('a, 'c) Either.t, ('b, 'c) Either.t) t
  (** Feed marked inputs through the argument arrow, passing the rest through
      unchanged to the output. *)
end

(** Minimal definition using [choose] operation without {!module:Arrow}. *)
module type WITH_CHOOSE = sig
  type ('a, 'b) t
  (** The type held by the [Arrow_choice]. *)

  val choose :
    ('a, 'b) t -> ('c, 'd) t -> (('a, 'c) Either.t, ('b, 'd) Either.t) t
  (** Split the input between the two argument arrows, retagging and merging
      their outputs. *)
end

(** Minimal definition using [choose] and [left] without {!module:Arrow}. *)
module type WITH_LEFT_AND_CHOOSE = sig
  include WITH_LEFT
  (** @inline *)

  include WITH_CHOOSE with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** Minimal definition using [arrow] and [fst] and [choose]. *)
module type WITH_ARROW_AND_FST_AND_CHOOSE = sig
  include WITH_CHOOSE
  (** @inline *)

  include Arrow.WITH_ARROW_AND_FST with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** Minimal definition using [arrow] and [fst] and [left]. *)
module type WITH_ARROW_AND_FST_AND_LEFT = sig
  include WITH_LEFT
  (** @inline *)

  include Arrow.WITH_ARROW_AND_FST with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** Minimal definition using [arrow] and [fst] and [choose]. *)
module type WITH_ARROW_AND_SPLIT_AND_CHOOSE = sig
  include WITH_CHOOSE
  (** @inline *)

  include Arrow.WITH_ARROW_AND_SPLIT with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** Minimal definition using [arrow] and [fst] and [left]. *)
module type WITH_ARROW_AND_SPLIT_AND_LEFT = sig
  include WITH_LEFT
  (** @inline *)

  include Arrow.WITH_ARROW_AND_SPLIT with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  include WITH_LEFT
  (** @inline *)

  include WITH_CHOOSE with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)

  include Arrow.CORE with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** Additional operations. *)
module type OPERATION = sig
  include Arrow.OPERATION
  (** @inline *)

  val right : ('a, 'b) t -> (('c, 'a) Either.t, ('c, 'b) Either.t) t
  (** The mirror image of [left]. *)

  val fan_in : ('a, 'c) t -> ('b, 'c) t -> (('a, 'b) Either.t, 'c) t
  (** Split the input between the two argument arrows and merge their outputs. *)
end

module type ALIAS = Arrow.ALIAS
(** Aliases of some operations functions. *)

(** Infix operators. *)
module type INFIX = sig
  include Arrow.INFIX
  (** @inline *)

  val ( +++ ) :
    ('a, 'b) t -> ('c, 'd) t -> (('a, 'c) Either.t, ('b, 'd) Either.t) t
  (** Infix version of {!val:CORE.choose}. *)

  val ( ||| ) : ('a, 'c) t -> ('b, 'c) t -> (('a, 'b) Either.t, 'c) t
  (** Infix version of {!val:CORE.fan_in}. *)
end

(** {1 Complete API} *)

(** The complete interface of an [Arrow_choice]. *)
module type API = sig
  (** {1 Type} *)

  type ('a, 'b) t
  (** The type held by the [Arrow_choice]. *)

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

    - {{:http://www.cse.chalmers.se/~rjmh/Papers/arrows.pdf} Generalising Monads
      to Arrows}
    - {{:https://www.haskell.org/arrows/} Arrows: A General Interface to
      Computation}
    - {{:https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Arrow.html}
      Haskell's documentation of Arrow} *)
