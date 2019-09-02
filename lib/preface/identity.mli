(** Exposes [Identity.t]. *)

type 'a t
(** A wrapper around a value. *)

val pure : 'a -> 'a t
(** Create a value from ['a] to ['a t]. *)

(** {1 Functor API} *)

module Functor : Specs.FUNCTOR with type 'a t = 'a t

include module type of Functor with type 'a t := 'a t
