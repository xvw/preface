(**An [Arrow_plus] is the conjonction of an {!module:Arrow_zero} and
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
module type COMBINE_AND_NEUTRAL = sig
  include Arrow_zero.NEUTRAL
  (** @closed *)

  include Arrow_alt.COMBINE with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** RMinimal definition using [arrow], [fst], [neutral] and [combine]. *)
module type CORE_WITH_ARROW_AND_FST = sig
  include COMBINE_AND_NEUTRAL
  (** @closed *)

  include Arrow.CORE_WITH_ARROW_AND_FST with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** Minimal definition using [arrow], [split], [neutral] and [combine]. *)
module type CORE_WITH_ARROW_AND_SPLIT = sig
  include COMBINE_AND_NEUTRAL
  (** @closed *)

  include Arrow.CORE_WITH_ARROW_AND_SPLIT with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** The minimum definition of an [Arrow_plus]. It is by using the combinators of
    this module that the other combinators will be derived. *)
module type CORE = sig
  include COMBINE_AND_NEUTRAL
  (** @closed *)

  include Arrow.CORE with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** Additional operations. *)
module type OPERATION = sig
  include Arrow_alt.OPERATION
  (** @closed *)

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
  (** {1 Core functions}

      Set of fundamental functions in the description of an [Arrow_plus]. *)

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
