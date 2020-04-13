(** Exposes [Stream.t], a kind of inifinite lists.

    {1 Capabilities}

    - {!val:Functor}
    - {!val:Applicative}
    - {!val:Monad}
    - {!val:Comonad}

    {1 Use cases}

    Using [Stream.t], we can define, for example, a stream with all positive
    natural numbers:

    {[
      let rec numbers n = stream n (lazy (numbers (n + 1)))

      let naturals = numbers 0
    ]}

    Or define all of fibonacci numbers:

    {[
      let rec fib_sum a b = stream (a + b) (lazy (fib_sum b (a + b))) in

      let fibonacci = 0 <:> stream 1 (lazy (fib_sum 0 1)) in

      let fib6th = fibonacci.%[6] (* Gives the 6th number of fibonacci. *)
    ]} *)

(** {1 Type} *)

type 'a t
(** A stream of ['a]. *)

(** {1 Implementation} *)

module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t
(** {2 Functor API} *)

module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t
(** {2 Applicative API} *)

module Monad : Preface_specs.MONAD with type 'a t = 'a t
(** {2 Monad API} *)

module Comonad : Preface_specs.COMONAD with type 'a t = 'a t
(** {2 Comonad API} *)

(** {1 Helpers} *)

val pure : 'a -> 'a t
(** Create a value from ['a] to ['a t]. *)

val repeat : 'a -> 'a t
(** Alias of {!val:pure}. [repeat x] creates a stream filled with [x]. *)

val stream : 'a -> 'a t Lazy.t -> 'a t
(** Build a stream manually. *)

val cons : 'a -> 'a t -> 'a t
(** Alias of {!val:stream}. *)

val hd : 'a t -> 'a
(** Get the head of the stream. Since [Stream.t] are infinites, [hd] can not
    fail (without values). For each stream, we have an head. *)

val tl : 'a t -> 'a t
(** Get the tail of the stream. Like [hd], since [Stream.t] are infinites, the
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

(** {1 Infix} *)

module Infix : sig
  val ( <:> ) : 'a -> 'a t -> 'a t
  (** Infix version of {!val:cons}*)

  val ( .%[] ) : 'a t -> int -> 'a Try.t
  (** Flipped version of {!val:at}. *)
end

include module type of Infix
