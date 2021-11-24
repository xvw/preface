(** Implementation for [Option.t]. *)

(** [Option.t] allows to explicitly describe the presence ([Some x]) or absence
    ([None]) of a value. This allows, among other things, the transformation of
    partial functions into total functions, and forces the explicit handling of
    the case where [None] is returned. *)

(** {1 Type} *)

type 'a t = 'a option

(** {1 Implementation}

    The set of concrete implementations for [Option.t]. *)

(** {2 Functor} *)

module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t

(** {2 Applicative}

    [Option.t] implements {!module-type:Preface_specs.APPLICATIVE} and
    introduces an interface to define {!module-type:Preface_specs.TRAVERSABLE}
    using [Option] as an iterable structure. *)

module Applicative :
  Preface_specs.Traversable.API_OVER_APPLICATIVE with type 'a t = 'a t

(** {2 Alternative} *)

module Alternative : Preface_specs.ALTERNATIVE with type 'a t = 'a t

(** {2 Monad}

    [Option.t] implements {!module-type:Preface_specs.MONAD} and introduces an
    interface to define {!module-type:Preface_specs.TRAVERSABLE} using [Option]
    as an iterable structure. *)

module Monad : Preface_specs.Traversable.API_OVER_MONAD with type 'a t = 'a t

(** {2 Monad Plus} *)

module Monad_plus : Preface_specs.MONAD_PLUS with type 'a t = 'a t

(** {2 Foldable} *)

module Foldable : Preface_specs.FOLDABLE with type 'a t = 'a t

(** {2 Invariant Functor} *)

module Invariant : Preface_specs.INVARIANT with type 'a t = 'a t

(** {2 Monoid}

    [Option] is the {e Free monoid over a semigroup} so wrapping a
    {!module-type:Preface_specs.SEMIGROUP} into an [Option] gives us a
    {!module-type:Preface_specs.MONOID} with [None] as a neutral element. *)

module Monoid (M : Preface_specs.SEMIGROUP) :
  Preface_specs.MONOID with type t = M.t t

(** {1 Addtional functions}

    Additional functions to facilitate practical work with [Option.t]. *)

val pure : 'a -> 'a t
(** Create a value from ['a] to ['a Option.t]. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Equality between [Option.t].*)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Formatter for pretty-printing for [Option.t]. *)

(** {2 Producing Option}*)

val if_ : 'a Predicate.t -> 'a -> 'a t
(** [is_ p x] produces [Some x] if [p x] is [true], otherwise, it produces
    [None]. *)

val unless : 'a Predicate.t -> 'a -> 'a t
(** [is_ p x] produces [Some x] if [p x] is [false], otherwise, it produces
    [None]. *)

(** {2 Composing Option}*)

val or_ : 'a t -> 'a t -> 'a t
(** [or_ a b] returns [a] if [a] is [Some x] else it returns [b]. Equivalent to
    [Alternative.combine]. *)
