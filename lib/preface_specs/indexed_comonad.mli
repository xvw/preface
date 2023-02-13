(** An [Indexed Comonad] is the dual of the {!module:Indexed_monad}. *)

(** {1 Minimal definition} *)

(** Minimal definition using [extract], [map] and [duplicate]. *)
module type WITH_MAP_AND_DUPLICATE = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Comonad]. *)

  val extract : ('a, 'index) t -> 'a
  (** Extract a ['a] from [('a, 'index) t]. Dual of return. *)

  val duplicate : ('a, 'index) t -> (('a, 'index) t, 'index) t
  (** Dual of join. *)

  val map : ('a -> 'b) -> ('a, 'index) t -> ('b, 'index) t
  (** Mapping over [t] from ['a] to ['b]. *)
end

(** Minimal definition using [extract] and [extend]. *)
module type WITH_EXTEND = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Comonad]. *)

  val extract : ('a, 'index) t -> 'a
  (** Extract a ['a] from [t]. Dual of return. *)

  val extend : (('a, 'index) t -> 'b) -> ('a, 'index) t -> ('b, 'index) t
  (** Dual of bind. *)
end

(** Minimal definition using [extract] and [compose_left_to_right]. *)
module type WITH_COKLEISLI_COMPOSITION = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Comonad]. *)

  val extract : ('a, 'index) t -> 'a
  (** Extract a ['a] from [t]. Dual of return. *)

  val compose_left_to_right :
    (('a, 'index) t -> 'b) -> (('b, 'index) t -> 'c) -> ('a, 'index) t -> 'c
  (** Composing monadic functions using Co-Kleisli Arrow (from left to right). *)
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  include WITH_MAP_AND_DUPLICATE
  (** @inline *)

  include WITH_EXTEND with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include WITH_COKLEISLI_COMPOSITION with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Additional operations. *)
module type OPERATION = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Comonad]. *)

  val lift : ('a -> 'b) -> ('a, 'index) t -> ('b, 'index) t
  (** Mapping over [t] from ['a] to ['b]. *)

  val lift2 :
    ('a -> 'b -> 'c) -> ('a, 'index) t -> ('b, 'index) t -> ('c, 'index) t
  (** Lift a binary function that acts on arbitrary values into a function that
      acts [t] values. *)

  val lift3 :
       ('a -> 'b -> 'c -> 'd)
    -> ('a, 'index) t
    -> ('b, 'index) t
    -> ('c, 'index) t
    -> ('d, 'index) t
  (** Lift a ternary function that acts on arbitrary values into a function that
      acts [t] values. *)

  val compose_right_to_left :
    (('b, 'index) t -> 'c) -> (('a, 'index) t -> 'b) -> ('a, 'index) t -> 'c
  (** Composing co-monadic functions using Co-Kleisli Arrow (from right to
      left). *)

  include Indexed_functor.OPERATION with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Syntax extensions. *)
module type SYNTAX = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Comonad]. *)

  val ( let@ ) : ('a, 'index) t -> (('a, 'index) t -> 'b) -> ('b, 'index) t
  (** Syntactic shortcuts for version of {!val:CORE.extend}:

      [let@ x = e in f] is equals to [extend f e]. *)

  include Indexed_functor.SYNTAX with type ('a, 'index) t := ('a, 'index) t
end

(** Infix operators. *)
module type INFIX = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Comonad]. *)

  val ( =>> ) : ('a, 'index) t -> (('a, 'index) t -> 'b) -> ('b, 'index) t
  (** Infix flipped version of {!val:CORE.extend}. *)

  val ( <<= ) : (('a, 'index) t -> 'b) -> ('a, 'index) t -> ('b, 'index) t
  (** Infix version of {!val:CORE.extend}. *)

  val ( =>= ) :
    (('a, 'index) t -> 'b) -> (('b, 'index) t -> 'c) -> ('a, 'index) t -> 'c
  (** Infix version of {!val:CORE.compose_left_to_right}. *)

  val ( =<= ) :
    (('b, 'index) t -> 'c) -> (('a, 'index) t -> 'b) -> ('a, 'index) t -> 'c
  (** Infix version of {!val:OPERATION.compose_right_to_left}. *)

  val ( <@@> ) : ('a, 'index) t -> ('a -> 'b, 'index) t -> ('b, 'index) t
  (** Applicative functor of [('a -> 'b) t] over [('a, 'index) t] to
      [('b, 'index) t]. *)

  val ( <@> ) : ('a -> 'b, 'index) t -> ('a, 'index) t -> ('b, 'index) t
  (** Applicative functor of [('a -> 'b) t] over [('a, 'index) t] to
      [('b, 'index) t]. *)

  val ( @> ) : (unit, 'index) t -> ('b, 'index) t -> ('b, 'index) t
  (** Discard the value of the first argument. *)

  val ( <@ ) : ('a, 'index) t -> (unit, 'index) t -> ('a, 'index) t
  (** Discard the value of the second argument. *)

  include Indexed_functor.INFIX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of an [Indexed Comonad]. *)
module type API = sig
  (** {1 Type} *)

  type ('a, 'index) t
  (** The type held by the [Indexed Comonad]. *)

  (** {1 Functions} *)

  include CORE with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include OPERATION with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type ('a, 'index) t := ('a, 'index) t

  include INFIX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  (** {1 Syntax} *)

  module Syntax : SYNTAX with type ('a, 'index) t := ('a, 'index) t

  include SYNTAX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end
