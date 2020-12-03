(** Exposes [Nonempty_list.t] a list with at least one element.

    {1 Capabilities}

    - {!val:Functor}
    - {!val:Applicative}
    - {!val:Selective}
    - {!val:Foldable}
    - {!val:Monad}
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

(** {2 Applicative API}

    Applicative and Traversable for [Nonempty_list.t]. *)
module Applicative : sig
  (** {2 Traversable using Applicative form} *)
  module Traversable (A : Preface_specs.APPLICATIVE) :
    Preface_specs.TRAVERSABLE with type 'a t = 'a A.t and type 'a iter = 'a t

  include Preface_specs.APPLICATIVE with type 'a t = 'a t
end

module Selective : Preface_specs.SELECTIVE with type 'a t = 'a t
(** {2 Selective API} *)

(** {2 Monad API}

    Monad and Traversable for [Nonempty_list.t].*)
module Monad : sig
  (** {2 Traversable using Monadic form} *)
  module Traversable (M : Preface_specs.MONAD) :
    Preface_specs.TRAVERSABLE with type 'a t = 'a M.t and type 'a iter = 'a t

  include Preface_specs.MONAD with type 'a t = 'a t
end

(** {2 Semigroup API} *)
module Semigroup (T : Preface_specs.Types.T0) :
  Preface_specs.SEMIGROUP with type t = T.t t

(** {1 Helpers} *)

val pure : 'a -> 'a t
(** Create a value from ['a] to ['a t]. *)

include module type of Preface_core.Nonempty_list with type 'a t := 'a t
