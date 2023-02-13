(** An {!module:Arrow} with a [combine] function. So [Arrow_alt] is also an
    {!module:Arrow}. [Arrow_alt] is a kind of {!module:Alt} in the arrow
    hierarchy.*)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Arrow_alt] must obey some
    laws.

    + All {!module:Arrow} laws
    + [combine (combine a b) c = combine a (combine b c)] *)

(** {1 Minimal definition} *)

(** Exposes the [combine] function, mandatory for each requirement. *)
module type WITH_COMBINE = sig
  type ('a, 'b) t
  (** The type held by the [Arrow_alt]. *)

  val combine : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  (** Combine two values of [('a, 'b) t] into one. *)
end

(** Minimal definition using [arrow], [fst] and [combine]. *)
module type WITH_ARROW_AND_FST = sig
  include WITH_COMBINE
  (** @inline *)

  include Arrow.WITH_ARROW_AND_FST with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** Minimal definition using [arrow], [split] and [combine]. *)
module type WITH_ARROW_AND_SPLIT = sig
  include WITH_COMBINE
  (** @inline *)

  include Arrow.WITH_ARROW_AND_SPLIT with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  include WITH_COMBINE
  (** @inline *)

  include Arrow.CORE with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** Additional operations. *)
module type OPERATION = sig
  include Arrow.OPERATION
  (** @inline *)

  val times_nel : int -> ('a, 'b) t -> ('a, 'b) t option
  (** [times n x] apply [combine] on [x] [n] times. If [n] is lower than [1] the
      function will returns [None]. *)

  val reduce_nel : ('a, 'b) t Preface_core.Nonempty_list.t -> ('a, 'b) t
  (** Reduce a [Nonempty_list.t] using [combine]. *)
end

module type ALIAS = Arrow.ALIAS
(** Aliases of some operations functions. *)

(** Infix operators. *)
module type INFIX = sig
  include Arrow.INFIX
  (** @inline *)

  val ( <|> ) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  (** Infix version of {!val:CORE.combine} *)
end

(** {1 Complete API} *)

(** The complete interface of an [Arrow_alt]. *)
module type API = sig
  (** {1 Type} *)

  type ('a, 'b) t
  (** The type held by the [Arrow_alt]. *)

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
