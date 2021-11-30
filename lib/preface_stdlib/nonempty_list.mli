(** Implementation for [Nonempty_list.t]. *)

(** A [Nonempty_list.t] is a list that contains at least one element. Therefore,
    the head and tails functions are total and safe. *)

(** {1 Type} *)

type 'a t = 'a Preface_core.Nonempty_list.t =
  | Last of 'a
  | ( :: ) of ('a * 'a t)

(** {1 Implementation} *)

(** {2 Functor} *)

module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t

(** {2 Alt} *)

module Alt : Preface_specs.ALT with type 'a t = 'a t

(** {2 Applicative}

    [Nonempty_list.t] implements {!module-type:Preface_specs.APPLICATIVE} and
    introduces an interface to define {!module-type:Preface_specs.TRAVERSABLE}
    using [Nonempty_list] as an iterable structure. *)

module Applicative :
  Preface_specs.Traversable.API_OVER_APPLICATIVE with type 'a t = 'a t

(** {2 Selective} *)

module Selective : Preface_specs.SELECTIVE with type 'a t = 'a t

(** {2 Monad}

    [Nonempty_list.t] implements {!module-type:Preface_specs.MONAD} and
    introduces an interface to define {!module-type:Preface_specs.TRAVERSABLE}
    using [Nonempty_list] as an iterable structure. *)

module Monad : Preface_specs.Traversable.API_OVER_MONAD with type 'a t = 'a t

(** {2 Comonad} *)

module Comonad : Preface_specs.COMONAD with type 'a t = 'a t

(** {2 Foldable} *)

module Foldable : Preface_specs.FOLDABLE with type 'a t = 'a t

(** {2 Invariant Functor} *)

module Invariant : Preface_specs.INVARIANT with type 'a t = 'a t

(** {2 Delayed implementation}

    By setting the [inner type] type of [Nonempty_list.t] it is possible to get
    implementations for abstractions without type parameter. *)

(** {3 Semigroup} *)

module Semigroup (T : Preface_specs.Types.T0) :
  Preface_specs.SEMIGROUP with type t = T.t t

(** {1 Additional functions}

    Additional functions to facilitate practical work with [Nonempty_list.t]. *)

val pure : 'a -> 'a t
(** Create a value from ['a] to ['a t]. *)

include module type of Preface_core.Nonempty_list with type 'a t := 'a t
(** @inline *)
