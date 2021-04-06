(** An [Arrow_apply] is an {!module:Arrow} with application capabilities. An
    [Arrow_apply] is also {!module:Arrow}. *)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Arrow_apply] must obey
    some laws.

    + All {!module:Arrow} laws
    + [fst (arrow (fun x -> arrow (fun y -> (x,y)))) >>> apply = id]
    + [fst (arrow (fun x -> g >>> x)) >>> apply = snd g >>> apply]
    + [fst (arrow (fun x -> x >>> h)) >>> apply = apply >>> h] *)

(** {1 Minimal definition} *)

(** Minimal definition using [apply] operation, without {!module:Arrow}. *)
module type WITH_APPLY = sig
  type ('a, 'b) t
  (** The type held by the [Arrow_apply]. *)

  val apply : (('a, 'b) t * 'a, 'b) t
  (** application of an arrow to an input.*)
end

(** Minimal definition using [arrow] and [fst] and [apply]. *)
module type WITH_ARROW_AND_FST = sig
  include WITH_APPLY
  (** @closed *)

  include Arrow.WITH_ARROW_AND_FST with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** Minimal definition using [arrow] and [fst] and [apply]. *)
module type WITH_ARROW_AND_SPLIT = sig
  include WITH_APPLY
  (** @closed *)

  include Arrow.WITH_ARROW_AND_SPLIT with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  include WITH_APPLY
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
