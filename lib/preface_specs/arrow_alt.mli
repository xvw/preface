(** An {!module:Arrow} with a [combine] function. So [Arrow_alt] is also an
    {!module:Arrow}. [Arrow_alt] is a kind of {!module:Alt} in the arrow
    hierarchy.*)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Arrow_alt] must obey some
    laws.

    + All {!module:Arrow} laws
    + [combine (combine a b) c = combine a (combine b c)] *)

(** {1 Structure anatomy} *)

(** Exposes the [combine] function, mandatory for each requirement. *)
module type COMBINE = sig
  type ('a, 'b) t
  (** The type held by the [Arrow_alt]. *)

  val combine : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  (** Combine two values of [('a, 'b) t] into one. *)
end

(** Minimal definition using [arrow], [fst] and [combine]. *)
module type CORE_WITH_ARROW_AND_FST = sig
  include COMBINE
  (** @closed *)

  include Arrow.CORE_WITH_ARROW_AND_FST with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** Minimal definition using [arrow], [split] and [combine]. *)
module type CORE_WITH_ARROW_AND_SPLIT = sig
  include COMBINE
  (** @closed *)

  include Arrow.CORE_WITH_ARROW_AND_SPLIT with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** The minimum definition of an [Arrow_alt]. It is by using the combinators of
    this module that the other combinators will be derived. *)
module type CORE = sig
  include COMBINE
  (** @closed *)

  include Arrow.CORE with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** Additional operations. *)
module type OPERATION = sig
  include Arrow.OPERATION
  (** @closed *)

  val times : int -> ('a, 'b) t -> ('a, 'b) t option
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
  (** @closed *)

  val ( <|> ) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  (** Infix version of {!val:CORE.combine} *)
end

(** {1 Complete API} *)

(** The complete interface of an [Arrow_alt]. *)
module type API = sig
  (** {1 Core functions}

      Set of fundamental functions in the description of an [Arrow_alt]. *)

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
