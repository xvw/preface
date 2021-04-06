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
  (** @closed *)

  include WITH_CHOOSE with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** Minimal definition using [arrow] and [fst] and [choose]. *)
module type WITH_ARROW_AND_FST_AND_CHOOSE = sig
  include WITH_CHOOSE
  (** @closed *)

  include Arrow.WITH_ARROW_AND_FST with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** Minimal definition using [arrow] and [fst] and [left]. *)
module type WITH_ARROW_AND_FST_AND_LEFT = sig
  include WITH_LEFT
  (** @closed *)

  include Arrow.WITH_ARROW_AND_FST with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** Minimal definition using [arrow] and [fst] and [choose]. *)
module type WITH_ARROW_AND_SPLIT_AND_CHOOSE = sig
  include WITH_CHOOSE
  (** @closed *)

  include Arrow.WITH_ARROW_AND_SPLIT with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** Minimal definition using [arrow] and [fst] and [left]. *)
module type WITH_ARROW_AND_SPLIT_AND_LEFT = sig
  include WITH_LEFT
  (** @closed *)

  include Arrow.WITH_ARROW_AND_SPLIT with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  include WITH_LEFT
  (** @closed *)

  include WITH_CHOOSE with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)

  include Arrow.CORE with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** Additional operations. *)
module type OPERATION = sig
  include Arrow.OPERATION
  (** @closed *)

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
  (** @closed *)

  val ( +++ ) :
    ('a, 'b) t -> ('c, 'd) t -> (('a, 'c) Either.t, ('b, 'd) Either.t) t
  (** Infix version of {!val:CORE.choose}. *)

  val ( ||| ) : ('a, 'c) t -> ('b, 'c) t -> (('a, 'b) Either.t, 'c) t
  (** Infix version of {!val:CORE.fan_in}. *)
end

(** {1 Complete API} *)

(** The complete interface of an [Arrow_choice]. *)
module type API = sig
  (** {1 Core functions}

      Set of fundamental functions in the description of an [Arrow_choice]. *)

  include CORE
  (** @closed *)

  (** {1 Additional functions}

      Additional functions, derived from fundamental functions. *)

  include OPERATION with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)

  (** {1 Aliases}

      Additional functions based on [Operation] mainly in order to be iso with
      Haskell convention. *)

  include ALIAS with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type ('a, 'b) t = ('a, 'b) t

  (** {2 Infix operators inclusion} *)

  include INFIX with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** {1 Additional references}

    - {{:http://www.cse.chalmers.se/~rjmh/Papers/arrows.pdf} Generalising Monads
      to Arrows}
    - {{:https://www.haskell.org/arrows/} Arrows: A General Interface to
      Computation}
    - {{:https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Arrow.html}
      Haskell's documentation of Arrow} *)
