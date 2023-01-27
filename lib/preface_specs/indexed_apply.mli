(** An [Indexed Apply] is an indexed functor with lifting and sequencing
    capabilities. [Indexed Apply] is more general (and by extension weaker) than
    a {!module:Indexed_applicative}. An [Indexed Apply] is also a
    {!module:Indexed_functor}. *)

(** {1 Minimal definition} *)

(** Minimal interface using [product]. *)
module type WITH_PRODUCT = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Apply]. *)

  val product : ('a, 'index) t -> ('b, 'index) t -> ('a * 'b, 'index) t
  (** Monoidal product between two [t]. *)
end

(** Minimal interface using [apply]. *)
module type WITH_APPLY = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Apply]. *)

  val apply : ('a -> 'b, 'index) t -> ('a, 'index) t -> ('b, 'index) t
  (** May apply a function wrapped into an [t] to a value also wrapped into an
      [t]. *)
end

(** Minimal interface using [lift2]. *)
module type WITH_LIFT2 = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Apply]. *)

  val lift2 :
    ('a -> 'b -> 'c) -> ('a, 'index) t -> ('b, 'index) t -> ('c, 'index) t
  (** Lift a binary function that acts on arbitrary values into a function that
      acts [t] values. *)
end

(** Minimal interface using [map] and [product]. *)
module type WITH_MAP_AND_PRODUCT = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Apply]. *)

  include Indexed_functor.WITH_MAP with type ('a, 'index) t := ('a, 'index) t
  include WITH_PRODUCT with type ('a, 'index) t := ('a, 'index) t
end

(** Minimal interface using [map] and [product]. *)
module type WITH_MAP_AND_APPLY = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Apply]. *)

  include Indexed_functor.WITH_MAP with type ('a, 'index) t := ('a, 'index) t
  include WITH_APPLY with type ('a, 'index) t := ('a, 'index) t
end

(** Minimal interface using [map] and [lift2]. *)
module type WITH_MAP_AND_LIFT2 = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Apply]. *)

  include Indexed_functor.WITH_MAP with type ('a, 'index) t := ('a, 'index) t
  include WITH_LIFT2 with type ('a, 'index) t := ('a, 'index) t
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Apply]. *)

  include Indexed_functor.WITH_MAP with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include WITH_APPLY with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include WITH_PRODUCT with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include WITH_LIFT2 with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Additional operations. *)
module type OPERATION = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Apply]. *)

  val lift : ('a -> 'b) -> ('a, 'index) t -> ('b, 'index) t
  (** Lift a ternary function that acts on arbitrary values into a function that
      acts [t] values. *)

  val lift3 :
       ('a -> 'b -> 'c -> 'd)
    -> ('a, 'index) t
    -> ('b, 'index) t
    -> ('c, 'index) t
    -> ('d, 'index) t
  (** (** Lift a quadnary function that acts on arbitrary values into a function
      that acts [t] values. *) *)

  include Indexed_functor.OPERATION with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Syntax extensions. *)
module type SYNTAX = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Apply]. *)

  include Indexed_functor.SYNTAX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  val ( and+ ) : ('a, 'index) t -> ('b, 'index) t -> ('a * 'b, 'index) t
  (** [and] operator for the monoidal product. *)
end

(** Infix operators. *)
module type INFIX = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Apply]. *)

  val ( <*> ) : ('a -> 'b, 'index) t -> ('a, 'index) t -> ('b, 'index) t
  (** [Applicative] functor of [('a -> 'b) t] over [('a, 'index) t] to
      [('b, 'index) t]. *)

  val ( <**> ) : ('a, 'index) t -> ('a -> 'b, 'index) t -> ('b, 'index) t
  (** Flipped [Applicative] functor of [('a -> 'b) t] over [('a, 'index) t] to
      [('b, 'index) t]. *)

  val ( *> ) : (unit, 'index) t -> ('a, 'index) t -> ('a, 'index) t
  (** Discard the value of the first argument. *)

  val ( <* ) : ('a, 'index) t -> (unit, 'index) t -> ('a, 'index) t
  (** Discard the value of the second argument. *)

  include Indexed_functor.INFIX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of an [Applicative]. *)
module type API = sig
  (** {1 Type} *)

  type ('a, 'index) t
  (** The type held by the [Indexed Apply]. *)

  (** {1 Functions} *)

  include CORE with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include OPERATION with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type ('a, 'index) t = ('a, 'index) t

  include module type of Infix with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  (** {1 Syntax} *)

  module Syntax : SYNTAX with type ('a, 'index) t = ('a, 'index) t

  include module type of Syntax with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end
