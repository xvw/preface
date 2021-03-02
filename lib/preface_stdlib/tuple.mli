(** Exposes [Tuple.t] a tuple over a and b.

    {1 Capabilities}

    - {!val:Bifunctor} *)

(** {1 Type} *)

type ('a, 'b) t = 'a * 'b

(** {1 Implementation} *)

module Bifunctor : Preface_specs.BIFUNCTOR with type ('a, 'b) t = ('a, 'b) t
(** {2 Bifunctor API} *)

(** {1 API} *)

(** {2 Extraction and manipulation} *)

val fst : ('a, 'b) t -> 'a
(** Extract the first component of a pair. *)

val snd : ('a, 'b) t -> 'b
(** Extract the second component of a pair. *)

val swap : ('a, 'b) t -> ('b, 'a) t
(** Swap both components *)

(** {2 For function application} *)

val curry : (('a, 'b) t -> 'c) -> 'a -> 'b -> 'c
(** Convert a function which take a tuple to a curried version. *)

val uncurry : ('a -> 'b -> 'c) -> ('a, 'b) t -> 'c
(** Convert a curried function to a function which take a tuple. *)

(** {2 infix operators} *)

module Infix : sig
  val ( & ) : 'a -> 'b -> ('a, 'b) t
end

include module type of Infix

(** {1 Helpers} *)

val equal :
  ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
(** Equality. *)

val pp :
     (Format.formatter -> 'a -> unit)
  -> (Format.formatter -> 'b -> unit)
  -> Format.formatter
  -> ('a, 'b) t
  -> unit
(** Pretty printing. *)
