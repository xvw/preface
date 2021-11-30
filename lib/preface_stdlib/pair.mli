(** Implementation for [Pair.t]. *)

(** [Pair.t] is the simplest product type. In addition to allowing the generic
    description of product types, it allows the description of a conjunction. *)

(** {1 Type} *)

type ('a, 'b) t = 'a * 'b

(** {1 Implementation} *)

(** {2 Bifunctor} *)

module Bifunctor : Preface_specs.BIFUNCTOR with type ('a, 'b) t = ('a, 'b) t

(** {1 Additional functions}

    Additional functions to facilitate practical work with [Pair.t]. *)

val fst : ('a, 'b) t -> 'a
(** Extract the first component of a pair. *)

val snd : ('a, 'b) t -> 'b
(** Extract the second component of a pair. *)

val swap : ('a, 'b) t -> ('b, 'a) t
(** Swap both components *)

val curry : (('a, 'b) t -> 'c) -> 'a -> 'b -> 'c
(** Convert a function which take a pair to a curried version. *)

val uncurry : ('a -> 'b -> 'c) -> ('a, 'b) t -> 'c
(** Convert a curried function to a function which take a pair. *)

val equal :
  ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
(** Equality between [Pair.t].*)

val pp :
     (Format.formatter -> 'a -> unit)
  -> (Format.formatter -> 'b -> unit)
  -> Format.formatter
  -> ('a, 'b) t
  -> unit
(** Formatter for pretty-printing for [Pair.t]. *)

(** {2 infix operators} *)

module Infix : sig
  val ( & ) : 'a -> 'b -> ('a, 'b) t
end

include module type of Infix
(** @inline *)
