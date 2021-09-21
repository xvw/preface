(** Implementation for [Seq.t]. *)

(** {1 Type} *)

type 'a t = 'a Stdlib.Seq.t

(** {1 Implementation} *)

(** {2 Functor} *)

module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t

(** {2 Applicative}

    [Seq.t] implements {!module-type:Preface_specs.APPLICATIVE} and introduces
    an interface to define {!module-type:Preface_specs.TRAVERSABLE} using [Seq]
    as an iterable structure. *)

module Applicative :
  Preface_specs.Traversable.API_OVER_APPLICATIVE with type 'a t = 'a t

(** {2 Alternative} *)

module Alternative : Preface_specs.ALTERNATIVE with type 'a t = 'a t

(** {2 Selective} *)

module Selective : Preface_specs.SELECTIVE with type 'a t = 'a t

(** {2 Monad}

    [Seq.t] implements {!module-type:Preface_specs.MONAD} and introduces an
    interface to define {!module-type:Preface_specs.TRAVERSABLE} using [Seq] as
    an iterable structure. *)

module Monad : Preface_specs.Traversable.API_OVER_MONAD with type 'a t = 'a t

(** {2 Monad Plus} *)

module Monad_plus : Preface_specs.MONAD_PLUS with type 'a t = 'a t

(** {2 Foldable} *)

module Foldable : Preface_specs.FOLDABLE with type 'a t = 'a t

(** {1 Addtional functions}

    Additional functions to facilitate practical work with [Seq.t]. *)

val pure : 'a -> 'a t
(** Create a value from ['a] to ['a t]. *)

val cons : 'a -> 'a t -> 'a t
(** [cons x xs] is the sequence containing the element [x] followed by the
    sequence [xs].*)

val rev : 'a t -> 'a t
(** Reverse a [Seq.t] (the function may not terminate if the sequence is
    infinite).*)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Equality between [Seq.t]. (The function may not terminate if the two
    sequence are infinite (and indentitcal)). *)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Formatter for pretty-printing for [Seq.t]. (The function may not terminate
    if the seq is infinite). *)
