(** Exposes [Identity.t].

    This module is mainly used as a "proof of concept" and as a test tool.

    {1 Capabilities}

    - {!val:Functor}
    - {!val:Applicative}
    - {!val:Selective}
    - {!val:Monad}
    - {!val:Comonad} *)

type 'a t
(** A wrapper around a value. *)

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

module Comonad : Preface_specs.COMONAD with type 'a t = 'a t
(** {2 Comonad API} *)

(** {1 Helpers} *)

val pure : 'a -> 'a t
(** Create a value from ['a] to ['a t]. *)

val extract : 'a t -> 'a
(** Create a value from ['a t] to ['a]. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Equality.*)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Pretty printing. *)
