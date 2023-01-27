(** An [Indexed Applicative] is an indexed functor with lifting and sequencing
    capabilities. [Applicative] is more general (and by extension weaker) than a
    {!module:Indexed_onad}. An [Indexed Applicative] is also a
    {!module:Indexed_functor}. *)

(** {1 Minimal definition} *)

(** Minimal interface using [map] and [product]. *)
module type WITH_PURE = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Applicative]. *)

  val pure : 'a -> ('a, 'index) t
  (** Lift a value from ['a] into a new [t]. *)
end

(** Minimal interface using [map] and [product]. *)
module type WITH_PURE_MAP_AND_PRODUCT = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Applicative]. *)

  include WITH_PURE with type ('a, 'index) t := ('a, 'index) t

  include
    Indexed_apply.WITH_MAP_AND_PRODUCT
      with type ('a, 'index) t := ('a, 'index) t
end

(** Minimal interface using [apply]. *)
module type WITH_PURE_AND_APPLY = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Applicative]. *)

  include WITH_PURE with type ('a, 'index) t := ('a, 'index) t
  include Indexed_apply.WITH_APPLY with type ('a, 'index) t := ('a, 'index) t
end

(** Minimal interface using [lift2]. *)
module type WITH_PURE_AND_LIFT2 = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Applicative]. *)

  include WITH_PURE with type ('a, 'index) t := ('a, 'index) t
  include Indexed_apply.WITH_LIFT2 with type ('a, 'index) t := ('a, 'index) t
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  include WITH_PURE_AND_APPLY
  (** @inline *)

  include WITH_PURE_MAP_AND_PRODUCT with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include WITH_PURE_AND_LIFT2 with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Additional operations. *)
module type OPERATION = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Applicative]. *)

  include Indexed_apply.OPERATION with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Infix operators. *)
module type INFIX = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Applicative]. *)

  include Indexed_apply.INFIX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Syntax extensions. *)
module type SYNTAX = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Applicative]. *)

  include Indexed_apply.SYNTAX with type ('a, 'index) t := ('a, 'index) t
end

(** {1 Complete API} *)

(** The complete interface of an [Indexed Applicative]. *)
module type API = sig
  (** {1 Type} *)

  type ('a, 'index) t
  (** The type held by the [Indexed Applicative]. *)

  (** {1 Functions} *)

  include CORE with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include OPERATION with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type ('a, 'index) t = ('a, 'index) t

  include INFIX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  (** {1 Syntax} *)

  module Syntax : SYNTAX with type ('a, 'index) t = ('a, 'index) t

  include SYNTAX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end
