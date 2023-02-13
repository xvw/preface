(** An [Arrow_plus] is the conjonction of an {!module:Arrow_zero} and
    {!module:Arrow_alt}. An [Arrow_plus] is a kind of {!module:Monoid} in the
    arrow hierarchy. And it also an {!module:Arrow_alt} and an
    {!module:Arrow_zero} (which is also a {!module:Arrow}). *)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Arrow_plus] must obey some
    laws.

    + All {!module:Arrow_alt} laws *)

(** {1 Structure anatomy} *)

(** Exposes the [neutral] value and [combine] function, mandatory for each
    requirement. *)
module type WITH_COMBINE_AND_NEUTRAL = sig
  include Arrow_zero.WITH_NEUTRAL
  (** @inline *)

  include Arrow_alt.WITH_COMBINE with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** RMinimal definition using [arrow], [fst], [neutral] and [combine]. *)
module type WITH_ARROW_AND_FST = sig
  include WITH_COMBINE_AND_NEUTRAL
  (** @inline *)

  include Arrow.WITH_ARROW_AND_FST with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** Minimal definition using [arrow], [split], [neutral] and [combine]. *)
module type WITH_ARROW_AND_SPLIT = sig
  include WITH_COMBINE_AND_NEUTRAL
  (** @inline *)

  include Arrow.WITH_ARROW_AND_SPLIT with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  include WITH_COMBINE_AND_NEUTRAL
  (** @inline *)

  include Arrow.CORE with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** Additional operations. *)
module type OPERATION = sig
  include Arrow_alt.OPERATION
  (** @inline *)

  val times : int -> ('a, 'b) t -> ('a, 'b) t
  (** [times_nel n x] apply [combine] on [x] [n] times. If [n] is lower than [1]
      the function will returns [neutral]. *)

  val reduce : ('a, 'b) t list -> ('a, 'b) t
  (** Reduce a [List.t] using [combine]. *)
end

module type ALIAS = Arrow.ALIAS
(** Aliases of some operations functions. *)

module type INFIX = Arrow_alt.INFIX
(** Infix operators. *)

(** {1 Complete API} *)

(** The complete interface of an [Arrow_plus]. *)
module type API = sig
  (** {1 Type} *)

  type ('a, 'b) t
  (** The type held by the [Arrow_plus]. *)

  (** {1 Functions} *)

  include CORE with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)

  include OPERATION with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type ('a, 'b) t := ('a, 'b) t

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
