(** An {!module:Arrow} with a [neutral] element. So [Arrow_zero] is also an
    {!module:Arrow}.*)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Arrow_zero] must obey some
    laws.

    + All {!module:Arrow} laws *)

(** {1 Structure anatomy} *)

(** Exposes the [neutral] value, mandatory for each requirement. *)
module type NEUTRAL = sig
  type ('a, 'b) t
  (** The type held by the [Arrow_zero]. *)

  val neutral : ('a, 'b) t
  (** The neutral element of the [Arrow_zero]. *)
end

(** Minimal definition using [arrow], [fst] and [neutral]. *)
module type CORE_WITH_ARROW_AND_FST = sig
  include NEUTRAL
  (** @closed *)

  include Arrow.CORE_WITH_ARROW_AND_FST with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** Minimal definition using [arrow], [split] and [neutral]. *)
module type CORE_WITH_ARROW_AND_SPLIT = sig
  include NEUTRAL
  (** @closed *)

  include Arrow.CORE_WITH_ARROW_AND_SPLIT with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** The minimum definition of an [Arrow_zero]. It is by using the combinators of
    this module that the other combinators will be derived. *)
module type CORE = sig
  include NEUTRAL
  (** @closed *)

  include Arrow.CORE with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

module type OPERATION = Arrow.OPERATION
(** Additional operations. *)

module type ALIAS = Arrow.ALIAS
(** Aliases of some operations functions. *)

module type INFIX = Arrow.INFIX
(** Infix operators. *)

(** {1 Complete API} *)

(** The complete interface of an [Arrow_zero]. *)
module type API = sig
  (** {1 Core functions}

      Set of fundamental functions in the description of an [Arrow]. *)

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
