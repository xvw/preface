(** An [Indexed Bind] is an [Indexed Monad] without [return] function. *)

(** {1 Minimal definition} *)

(** Minimal definition using [bind]. *)
module type WITH_BIND = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Bind]. *)

  val bind : ('a -> ('b, 'index) t) -> ('a, 'index) t -> ('b, 'index) t
  (** [bind f m] passes the result of computation [m] to function [f]. *)
end

(** Minimal definition using [map] and [join]. *)
module type WITH_MAP_AND_JOIN = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Bind]. *)

  val map : ('a -> 'b) -> ('a, 'index) t -> ('b, 'index) t
  (** Mapping over from ['a] to ['b]. *)

  val join : (('a, 'index) t, 'index) t -> ('a, 'index) t
  (** [join] remove one level of monadic structure, projecting its bound
      argument into the outer level. *)
end

(** Minimal definition using [compose_left_to_right]. *)
module type WITH_KLEISLI_COMPOSITION = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Bind]. *)

  val compose_left_to_right :
    ('a -> ('b, 'index) t) -> ('b -> ('c, 'index) t) -> 'a -> ('c, 'index) t
  (** Composing monadic functions using Kleisli Arrow (from left to right). *)
end

(** Minimal definition using [map] and [bind]. *)
module type WITH_MAP_AND_BIND = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Bind]. *)

  include Indexed_functor.WITH_MAP with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include WITH_BIND with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Minimal definition using [map] and [compose_left_to_right]. *)
module type WITH_MAP_AND_KLEISLI_COMPOSITION = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Bind]. *)

  include Indexed_functor.WITH_MAP with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include WITH_KLEISLI_COMPOSITION with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  include WITH_BIND
  (** @inline *)

  include WITH_MAP_AND_JOIN with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include WITH_KLEISLI_COMPOSITION with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Additional operations. *)
module type OPERATION = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Bind]. *)

  val compose_right_to_left :
    ('b -> ('c, 'index) t) -> ('a -> ('b, 'index) t) -> 'a -> ('c, 'index) t
  (** Composing monadic functions using Kleisli Arrow (from right to left). *)

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

  include Indexed_functor.OPERATION with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Syntax extensions. *)
module type SYNTAX = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Bind]. *)

  val ( let* ) : ('a, 'index) t -> ('a -> ('b, 'index) t) -> ('b, 'index) t
  (** Syntactic shortcuts for flipped version of {!val:CORE.bind}:

      [let* x = e in f] is equals to [bind (fun x -> f) e]. *)

  include Indexed_functor.SYNTAX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Infix operators. *)
module type INFIX = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Bind]. *)

  val ( =|< ) : ('a -> 'b) -> ('a, 'index) t -> ('b, 'index) t
  (** Infix version of {!val:CORE.map}. *)

  val ( >|= ) : ('a, 'index) t -> ('a -> 'b) -> ('b, 'index) t
  (** Infix flipped version of {!val:CORE.map}. *)

  val ( >>= ) : ('a, 'index) t -> ('a -> ('b, 'index) t) -> ('b, 'index) t
  (** Infix flipped version of {!val:CORE.bind}. *)

  val ( =<< ) : ('a -> ('b, 'index) t) -> ('a, 'index) t -> ('b, 'index) t
  (** Infix version of {!val:CORE.bind}. *)

  val ( >=> ) :
    ('a -> ('b, 'index) t) -> ('b -> ('c, 'index) t) -> 'a -> ('c, 'index) t
  (** Infix version of {!val:CORE.compose_left_to_right}. *)

  val ( <=< ) :
    ('b -> ('c, 'index) t) -> ('a -> ('b, 'index) t) -> 'a -> ('c, 'index) t
  (** Infix version of {!val:OPERATION.compose_right_to_left}. *)

  val ( >> ) : (unit, 'index) t -> ('b, 'index) t -> ('b, 'index) t
  (** Sequentially compose two actions, discarding any value produced by the
      first. *)

  val ( << ) : ('a, 'index) t -> (unit, 'index) t -> ('a, 'index) t
  (** Sequentially compose two actions, discarding any value produced by the
      second. *)

  include Indexed_functor.INFIX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of a [Monad]. *)
module type API = sig
  (** {1 Type} *)

  type ('a, 'index) t
  (** The type held by the [Monad]. *)

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
