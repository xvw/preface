(** Build monoidal approximation. *)

(** [Over] and [Under] allows Static analysis of selective functors with
    over-approximation.(mentionned in
    {{:https://dl.acm.org/doi/pdf/10.1145/3341694} Selective Applicative
    Functor} by A. Mokhov, G. Lukyanov, S. Marlow and J. Dimino. *)

(** {1 Over approximation}

    The selective functor [Over] can be used for computing a [list] of all
    effects embedded in a computation, i.e. an "over-approximation" of the
    effects that will actually occur. This is achieved by keeping track of
    effects in both arguments of the select operator. *)

module Over (M : Preface_specs.MONOID) : sig
  (** {1 Type} *)

  type _ t = Over of M.t

  (** {1 Implementation} *)

  (** {2 Applicative} *)

  module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t

  (** {2 Selective} *)

  module Selective : Preface_specs.SELECTIVE with type 'a t = 'a t

  (** {1 Addtional functions}

      Additional functions to facilitate practical work with [Over.t]. *)

  val get : 'a t -> M.t
  (** Retreive the [Monoid] value from the [Over] approximation. *)
end

(** {1 Under approximation}

    The selective functor [Under] discards the second argument of [select], and
    therefore computes an "under-approximation", i.e. a list of effects that are
    guaranteed to occur. *)

module Under (M : Preface_specs.MONOID) : sig
  (** {1 Type} *)

  type _ t = Under of M.t

  (** {1 Implementation} *)

  (** {2 Applicative} *)

  module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t

  (** {2 Selective} *)

  module Selective : Preface_specs.SELECTIVE with type 'a t = 'a t

  (** {1 Addtional functions}

      Additional functions to facilitate practical work with [Under.t]. *)

  val get : 'a t -> M.t
  (** Retreive the [Monoid] value from the [Under] approximation. *)
end
