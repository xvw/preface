(** Exposes [Identity.t]. *)

type 'a t
(** A wrapper around a value. *)

val pure : 'a -> 'a t
(** Create a value from ['a] to ['a t]. *)

(** {1 Functor API} *)

module Functor : Specs.FUNCTOR with type 'a t = 'a t

(*include module type of Functor with type 'a t := 'a t*)

(** {1 Applicative API} *)

module Applicative : Specs.APPLICATIVE with type 'a t = 'a t

(*include module type of Applicative with type 'a t := 'a t*)

(** {1 Selective API} *)

module Selective (E : Specs.EITHER) : Specs.SELECTIVE with type 'a t = 'a t

(** {1 Monad API} *)

module Monad : Specs.MONAD with type 'a t = 'a t

(*include module type of Monad with type 'a t := 'a t*)

(** {2 Helpers} *)

val eq : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Equality.*)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Pretty printing. *)
