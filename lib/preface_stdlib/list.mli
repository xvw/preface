(** Exposes [List.t]

    {1 Capabilities}

    - {!val:Functor}
    - {!val:Applicative}
    - {!val:Alternative}
    - {!val:Selective}
    - {!val:Monad}
    - {!val:Monad_plus}
    - {!val:Foldable}
    - [Traversable] for {!val:Applicative} and {!val:Monad}
    - {!val:Monoid} *)

(** {1 Type} *)

type 'a t = 'a list

(** {1 Implementation} *)

module Foldable : Preface_specs.FOLDABLE with type 'a t = 'a t
(** {2 Foldable API} *)

module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t
(** {2 Functor API} *)

module Applicative :
  Preface_specs.Traversable.API_OVER_APPLICATIVE with type 'a t = 'a t
(** {2 Applicative API}

    Applicative and Traversable for [List.t]. *)

module Alternative : Preface_specs.ALTERNATIVE with type 'a t = 'a t
(** {2 Alternative API} *)

module Selective : Preface_specs.SELECTIVE with type 'a t = 'a t
(** {2 Selective API} *)

module Monad : Preface_specs.Traversable.API_OVER_MONAD with type 'a t = 'a t
(** {2 Monad API}

    Monad and Traversable for [List.t]. The implementation of [bind] come from
    [concat_map] in OCaml's Stdlib (available since OCaml 4.10) *)

module Monad_plus : Preface_specs.MONAD_PLUS with type 'a t = 'a t
(** {2 Alternative API} *)

(** {2 Monoid API} *)
module Monoid (T : Preface_specs.Types.T0) :
  Preface_specs.MONOID with type t = T.t t

(** {1 Helpers} *)

val pure : 'a -> 'a t
(** Create a value from ['a] to ['a t]. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Equality.*)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Pretty printing. *)
