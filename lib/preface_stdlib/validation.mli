(** Implementation for [Validation.t]. *)

(** [Validation] is very similar to [Result], the main difference between the
    two lies in the (delayed) implementation of
    {!module-type:Preface_specs.APPLICATIVE} which allows, unlike Result, the
    accumulation of errors. *)

(** {1 Type} *)

type ('a, 'errors) t =
  | Valid of 'a
  | Invalid of 'errors

(** {1 Implementation} *)

(** {2 Bifunctor} *)

module Bifunctor : Preface_specs.BIFUNCTOR with type ('a, 'b) t = ('a, 'b) t

(** {2 Delayed implementation}

    By setting the [error] type of [Validation.t] it is possible to get
    implementations for abstractions on constructors of type with an arity of 1. *)

(** {3 Functor} *)

module Functor (T : Preface_specs.Types.T0) :
  Preface_specs.FUNCTOR with type 'a t = ('a, T.t) t

(** {3 Alt} *)

module Alt (Errors : Preface_specs.SEMIGROUP) :
  Preface_specs.ALT with type 'a t = ('a, Errors.t) t

(** {3 Applicative}

    [Validation.t] implements {!module-type:Preface_specs.APPLICATIVE} and
    introduces an interface to define {!module-type:Preface_specs.TRAVERSABLE}
    using [Validation] as an iterable structure.

    As you can see, it is in the definition of the
    {!module-type:Preface_specs.APPLICATIVE} that [Validation] differs from
    [Result]. The ['errors] part must be a
    {!module-type:Preface_specs.SEMIGROUP} to allow for the accumulation of
    errors. *)

module Applicative (Errors : Preface_specs.SEMIGROUP) :
  Preface_specs.Traversable.API_OVER_APPLICATIVE
    with type 'a t = ('a, Errors.t) t

(** {3 Selective} *)

module Selective (Errors : Preface_specs.SEMIGROUP) :
  Preface_specs.SELECTIVE with type 'a t = ('a, Errors.t) t

(** {3 Monad}

    [Validation.t] implements {!module-type:Preface_specs.MONAD} and introduces
    an interface to define {!module-type:Preface_specs.TRAVERSABLE} using
    [Validation] as an iterable structure. *)

module Monad (T : Preface_specs.Types.T0) :
  Preface_specs.Traversable.API_OVER_MONAD with type 'a t = ('a, T.t) t

(** {3 Foldable} *)

module Foldable (T : Preface_specs.Types.T0) :
  Preface_specs.FOLDABLE with type 'a t = ('a, T.t) t

(** {3 Invariant} *)

module Invariant (T : Preface_specs.Types.T0) :
  Preface_specs.INVARIANT with type 'a t = ('a, T.t) Bifunctor.t

(** {1 Additional functions}

    Additional functions to facilitate practical work with [Validation.t]. *)

val valid : 'a -> ('a, 'b) t
(** Wrap a value into [Valid].*)

val invalid : 'b -> ('a, 'b) t
(** Wrap an error value [Invalid]. *)

val pure : 'a -> ('a, 'b) t
(** Alias for [valid]. *)

val case : ('a -> 'c) -> ('b -> 'c) -> ('a, 'b) t -> 'c
(** [case f g x] apply [f] if [x] is [Valid], [g] if [x] is [Invalid].*)

val equal :
  ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
(** Equality between [Validation.t].*)

val pp :
     (Format.formatter -> 'a -> unit)
  -> (Format.formatter -> 'b -> unit)
  -> Format.formatter
  -> ('a, 'b) t
  -> unit
(** Formatter for pretty-printing for [Validation.t]. *)
