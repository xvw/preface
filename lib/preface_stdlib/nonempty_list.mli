(** Exposes [Nonempty_list.t] a list with at least one element.

    {1 Capabilities}

    - {!val:Functor}
    - {!val:Applicative} with Traversable module
    - {!val:Selective}
    - {!val:Foldable}
    - {!val:Monad} with Traversable module
    - [Traversable] for {!val:Applicative} and {!val:Monad}
    - {!val:Semigroup} *)

(** {1 Type} *)

type 'a t = 'a Preface_core.Nonempty_list.t =
  | Last of 'a
  | ( :: ) of ('a * 'a t)

(** {1 Implementation} *)

module Alt : Preface_specs.ALT with type 'a t = 'a t
(** {2 Alt API} *)

module Foldable : Preface_specs.FOLDABLE with type 'a t = 'a t
(** {2 Foldable API} *)

module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t
(** {2 Functor API} *)

module Applicative :
  Preface_specs.Traversable.API_OVER_APPLICATIVE with type 'a t = 'a t
(** {2 Applicative API}

    Applicative and Traversable for [Nonempty_list.t]. *)

module Selective : Preface_specs.SELECTIVE with type 'a t = 'a t
(** {2 Selective API} *)

module Monad : Preface_specs.Traversable.API_OVER_MONAD with type 'a t = 'a t
(** {2 Monad API}

    Monad and Traversable for [Nonempty_list.t].*)

(** {2 Semigroup API} *)
module Semigroup (T : Preface_specs.Types.T0) :
  Preface_specs.SEMIGROUP with type t = T.t t

(** {1 Helpers} *)

val pure : 'a -> 'a t
(** Create a value from ['a] to ['a t]. *)

include module type of Preface_core.Nonempty_list with type 'a t := 'a t
