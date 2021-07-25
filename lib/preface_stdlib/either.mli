(** Implementation for [Either.t]. *)

(** [Either.t] is the simplest sum type. In addition to allowing the generic
    description of sum types, it allows the description of a disjunction, for
    example to generalise a conditional branching. *)

(** {1 Type} *)

type ('a, 'b) t = ('a, 'b) Preface_core.Shims.Either.t =
  | Left of 'a
  | Right of 'b

(** {1 Implementation}

    The set of concrete implementations for [Either.t]. *)

(** {2 Bifunctor} *)

module Bifunctor : Preface_specs.BIFUNCTOR with type ('a, 'b) t = ('a, 'b) t

(** {2 Delayed implementation}

    By setting the [left] type of [Either.t] it is possible to get
    implementations for abstractions on constructors of type with an arity of 1. *)

(** {3 Functor} *)

module Functor (T : Preface_specs.Types.T0) :
  Preface_specs.FUNCTOR with type 'a t = (T.t, 'a) Bifunctor.t

(** {3 Alt} *)

module Alt (T : Preface_specs.Types.T0) :
  Preface_specs.ALT with type 'a t = (T.t, 'a) Bifunctor.t

(** {3 Applicative}

    [Either.t] implements {!module-type:Preface_specs.APPLICATIVE} and
    introduces an interface to define {!module-type:Preface_specs.TRAVERSABLE}
    using [Either] as an iterable structure. *)

module Applicative (T : Preface_specs.Types.T0) :
  Preface_specs.Traversable.API_OVER_APPLICATIVE
    with type 'a t = (T.t, 'a) Bifunctor.t

(** {3 Monad}

    [Either.t] implements {!module-type:Preface_specs.MONAD} and introduces an
    interface to define {!module-type:Preface_specs.TRAVERSABLE} using [Either]
    as an iterable structure. *)

module Monad (T : Preface_specs.Types.T0) :
  Preface_specs.Traversable.API_OVER_MONAD
    with type 'a t = (T.t, 'a) Bifunctor.t

(** {3 Foldable} *)

module Foldable (T : Preface_specs.Types.T0) :
  Preface_specs.FOLDABLE with type 'a t = (T.t, 'a) Bifunctor.t

(** {1 Addtional functions}

    Additional functions to facilitate practical work with [Either.t]. *)

val pure : 'b -> ('a, 'b) t
(** Create a value from ['b] to [('a, 'b) t], a value wrapped in [Right]. *)

val equal :
  ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
(** Equality between [Either.t].*)

val pp :
     (Format.formatter -> 'a -> unit)
  -> (Format.formatter -> 'b -> unit)
  -> Format.formatter
  -> ('a, 'b) t
  -> unit
(** Formatter for pretty-printing for [Either.t]. *)
