type 'a t = 'a option

(** {1 Functor API} *)

module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t

(** {1 Applicative API} *)

module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t

(** {1 Monad API} *)

module Monad : Preface_specs.MONAD with type 'a t = 'a t

(** {2 Helpers} *)

val eq : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Equality.*)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Pretty printing. *)
