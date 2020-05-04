(** Exposes [Nonempty_list.t] a list with at least one element.

    {1 Capabilities}

    - {!val:Functor}
    - {!val:Applicative}
    - {!val:Selective}
    - {!val:Monad} *)

include module type of Preface_core.Nonempty_list

(** {1 Implementation} *)

module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t
(** {2 Functor API} *)

module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t
(** {2 Applicative API} *)

(** {2 Selective API} *)
module Selective :
  Preface_specs.SELECTIVE
    with type 'a t = 'a t
     and type ('a, 'b) either = ('a, 'b) Preface_core.Either.t

module Monad : Preface_specs.MONAD with type 'a t = 'a t
(** {2 Monad API} *)

(** {1 Helpers} *)

val pure : 'a -> 'a t
(** Create a value from ['a] to ['a t]. *)
