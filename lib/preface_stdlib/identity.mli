(** Exposes [Identity.t].

    This module is mainly used as a "proof of concept" and as a test tool.

    {1 Capabilities}

    - {!val:Functor}
    - {!val:Applicative}
    - {!val:Monad} *)

type 'a t
(** A wrapper around a value. *)

(** {1 Implementation} *)

module Functor : Preface_specs.FUNCTOR with type 'a t := 'a t
(** {2 Functor API} *)

module Applicative : Preface_specs.APPLICATIVE with type 'a t := 'a t
(** {2 Applicative API} *)

module Monad : Preface_specs.MONAD with type 'a t = 'a t
(** {2 Monad API} *)

(** {1 Helpers} *)

val pure : 'a -> 'a t
(** Create a value from ['a] to ['a t]. *)

val eq : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Equality.*)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Pretty printing. *)
