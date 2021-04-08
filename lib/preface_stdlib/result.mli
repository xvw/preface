(** Implementation for [Result.t]. *)

(** [Result.t] is like [Either.t] but it exist for semantic reasons. ([Left] and
    [Right] are not very specific on what are the valid and the invalid branch). *)

(** {1 Type} *)

type ('a, 'b) t = ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

(** {1 Implementation} *)

(** {2 Bifunctor} *)

module Bifunctor : Preface_specs.BIFUNCTOR with type ('a, 'b) t = ('a, 'b) t
(** {2 Bifunctor API} *)

(** {2 Delayed implementation}

    By setting the [error] type of [Result.t] it is possible to get
    implementations for abstractions on constructors of type with an arity of 1. *)

(** {3 Functor} *)

module Functor (T : Preface_specs.Types.T0) :
  Preface_specs.FUNCTOR with type 'a t = ('a, T.t) Bifunctor.t

(** {3 Applicative}

    [Result.t] implements {!module-type:Preface_specs.APPLICATIVE} and
    introduces an interface to define {!module-type:Preface_specs.TRAVERSABLE}
    using [Result] as an iterable structure. *)

module Applicative (T : Preface_specs.Types.T0) :
  Preface_specs.Traversable.API_OVER_APPLICATIVE
    with type 'a t = ('a, T.t) Bifunctor.t

(** {3 Monad}

    [Result.t] implements {!module-type:Preface_specs.MONAD} and introduces an
    interface to define {!module-type:Preface_specs.TRAVERSABLE} using [Result]
    as an iterable structure. *)

module Monad (T : Preface_specs.Types.T0) :
  Preface_specs.Traversable.API_OVER_MONAD
    with type 'a t = ('a, T.t) Bifunctor.t

(** {1 Addtional functions}

    Additional functions to facilitate practical work with [Result.t]. *)

val pure : 'a -> ('a, 'b) t
(** Create a value from ['b] to [('a, 'b) t]. *)

val equal :
  ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
(** Equality between [Result.t].*)

val pp :
     (Format.formatter -> 'a -> unit)
  -> (Format.formatter -> 'b -> unit)
  -> Format.formatter
  -> ('a, 'b) t
  -> unit
(** Formatter for pretty-printing for [Result.t]. *)
