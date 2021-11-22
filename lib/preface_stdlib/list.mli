(** Implementation for [List.t]. *)

(** {1 Type} *)

type 'a t = 'a list

(** {1 Implementation} *)

(** {2 Functor} *)

module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t

(** {2 Applicative}

    [List.t] implements {!module-type:Preface_specs.APPLICATIVE} and introduces
    an interface to define {!module-type:Preface_specs.TRAVERSABLE} using [List]
    as an iterable structure. *)

module Applicative :
  Preface_specs.Traversable.API_OVER_APPLICATIVE with type 'a t = 'a t

(** {2 Alternative} *)

module Alternative : Preface_specs.ALTERNATIVE with type 'a t = 'a t

(** {2 Selective} *)

module Selective : Preface_specs.SELECTIVE with type 'a t = 'a t

(** {2 Monad}

    [List.t] implements {!module-type:Preface_specs.MONAD} and introduces an
    interface to define {!module-type:Preface_specs.TRAVERSABLE} using [List] as
    an iterable structure. *)

module Monad : Preface_specs.Traversable.API_OVER_MONAD with type 'a t = 'a t

(** {2 Monad Plus} *)

module Monad_plus : Preface_specs.MONAD_PLUS with type 'a t = 'a t

(** {2 Foldable} *)

module Foldable : Preface_specs.FOLDABLE with type 'a t = 'a t

(** {2 Invariant Functor} *)

module Invariant : Preface_specs.INVARIANT with type 'a t = 'a t

(** {2 Monoid}

    [List] is the {e Free monoid over a type} so wrapping a
    {!module-type:Preface_specs.SEMIGROUP} into an [List] gives us a
    {!module-type:Preface_specs.MONOID} with [empty list] as a neutral element. *)

module Monoid (T : Preface_specs.Types.T0) :
  Preface_specs.MONOID with type t = T.t t

(** {1 Addtional functions}

    Additional functions to facilitate practical work with [List.t]. *)

val pure : 'a -> 'a t
(** Create a value from ['a] to ['a t]. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Equality between [List.t].*)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Formatter for pretty-printing for [List.t]. *)
