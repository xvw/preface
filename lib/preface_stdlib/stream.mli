(** Implementation for [Stream.t]. *)

(** A [Stream.t] is an infinite lazy list. *)

(** {1 Type} *)

type 'a t

(** {1 Implementation} *)

(** {2 Functor} *)

module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t

(** {2 Applicative} *)

module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t

(** {2 Monad} *)

module Monad : Preface_specs.MONAD with type 'a t = 'a t

(** {2 Comonad} *)

module Comonad : Preface_specs.COMONAD with type 'a t = 'a t

(** {2 Invariant} *)

module Invariant : Preface_specs.INVARIANT with type 'a t = 'a t

(** {1 Additional functions}

    Additional functions to facilitate practical work with [Stream.t]. *)

val pure : 'a -> 'a t
(** Create a value from ['a] to ['a t]. *)

val repeat : 'a -> 'a t
(** Alias of {!val:pure}. [repeat x] creates a stream filled with [x]. *)

val stream : 'a -> 'a t Lazy.t -> 'a t
(** Build a stream manually. *)

val cons : 'a -> 'a t -> 'a t
(** Alias of {!val:stream}. *)

val hd : 'a t -> 'a
(** Get the head of the stream. Since [Stream.t] are infinite, [hd] can not fail
    (without values). For each stream, we have an head. *)

val tl : 'a t -> 'a t
(** Get the tail of the stream. Like [hd], since [Stream.t] are infinite, the
    function can not fail. For each stream, we have a tail.*)

val at : int -> 'a t -> 'a Try.t
(** Get the value at given position. [at 10 (naturals)] gives [Ok 9]. The
    function may fail if the position if negative. *)

val drop : int -> 'a t -> 'a t Try.t
(** [drop n stream] drop the [n]th values of the [stream]. The function may fail
    if the position if negative.*)

val take : int -> 'a t -> 'a list Try.t
(** [take n stream] returns the firsts [n] elements of [stream] as [list]. *)

val take_while : ('a -> bool) -> 'a t -> 'a list
(** [take_while p stream] returns all first elements of [stream] as [list]
    satisfying the predicate [p]. Be careful, the function may diverge if every
    element of the stream are satisfying the predicate [p].*)

val drop_while : ('a -> bool) -> 'a t -> 'a t
(** [drop_while p stream] drop all first elements of [stream] satisfying the
    predicate [p]. Be careful, the function may diverge if every element of the
    stream are satisfying the predicate [p].*)

(** {2 infix operators} *)

module Infix : sig
  val ( <:> ) : 'a -> 'a t -> 'a t
  (** Infix version of {!val:cons}*)

  val ( .%[] ) : 'a t -> int -> 'a Try.t
  (** Flipped version of {!val:at}. *)
end

include module type of Infix
(** @inline *)
