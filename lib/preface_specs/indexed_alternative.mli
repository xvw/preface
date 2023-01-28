(** [Indexed Alternative] is a kind of {!module:Monoid} on
    {!module:Indexed_applicative}. An [Indexed_alternative] is formally an
    {!module:Indexed_pplicative} with [neutral] and [combine]. So an
    [Indexed_alternative] is also an {!module:Indexed_applicative} (and an
    {!module:Indexed_alt} which is also a {!module:Indexed_functor}).*)

(** {1 Minimal definition} *)

(** Minimal interfaces of [Alternative] without {!module:Applicative}. *)
module type WITH_NEUTRAL_AND_COMBINE = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Alternative]. *)

  include Indexed_alt.WITH_COMBINE with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  val neutral : ('a, 'index) t
  (** The neutral element of the [t]. *)
end

(** Minimal definition using [neutral], [combine], [pure], [map] and [product]. *)
module type WITH_PURE_MAP_AND_PRODUCT = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Alternative]. *)

  include
    Indexed_applicative.WITH_PURE_MAP_AND_PRODUCT
      with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include WITH_NEUTRAL_AND_COMBINE with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Minimal definition using [neutral], [combine], [pure] and [apply]. *)
module type WITH_PURE_AND_APPLY = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Alternative]. *)

  include
    Indexed_applicative.WITH_PURE_AND_APPLY
      with type ('a, 'index) t := ('a, 'index) t

  include WITH_NEUTRAL_AND_COMBINE with type ('a, 'index) t := ('a, 'index) t
end

(** Minimal definition using [neutral], [combine], [pure] and [lift2]. *)
module type WITH_PURE_AND_LIFT2 = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Alternative]. *)

  include
    Indexed_applicative.WITH_PURE_AND_LIFT2
      with type ('a, 'index) t := ('a, 'index) t

  include WITH_NEUTRAL_AND_COMBINE with type ('a, 'index) t := ('a, 'index) t
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  include WITH_NEUTRAL_AND_COMBINE
  (** @inline *)

  include Indexed_applicative.CORE with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Operation without {!module:Indexed_applicative}. *)
module type ALTERNATIVE_OPERATION = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Alternative]. *)

  include Indexed_alt.OPERATION with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  val times : int -> ('a, 'index) t -> ('a, 'index) t
  (** [times n x] apply [combine] on [x] [n] times. If [n] is lower than [1] the
      function will returns [neutral]. *)

  val reduce : ('a, 'index) t list -> ('a, 'index) t
  (** Reduce a [List.t] using [combine]. *)
end

(** Additional operations. *)
module type OPERATION = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Alternative]. *)

  include
    Indexed_applicative.OPERATION with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include ALTERNATIVE_OPERATION with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Infix operators. *)
module type INFIX = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Alternative]. *)

  include Indexed_applicative.INFIX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include Indexed_alt.INFIX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Syntax extensions. *)
module type SYNTAX = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Alternative]. *)

  include Indexed_applicative.SYNTAX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of an [Indexed Alternative]. *)
module type API = sig
  (** {1 Type} *)

  type ('a, 'index) t
  (** The type held by the [Indexed Alternative]. *)

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
