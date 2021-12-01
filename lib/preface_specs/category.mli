(** A [Category] is a [Semigroupoid] with an identity at each object. *)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Category] must obey some
    laws.

    + [f % id = f]
    + [id % f = f]
    + [f % (g % h) = (f % g) % h] *)

(** {1 Minimal definition} *)

(** Minimal interface using [id]. *)
module type WITH_ID = sig
  type ('a, 'b) t
  (** The type held by the [Category]. *)

  val id : ('a, 'a) t
  (** The identity morphism. *)
end

(** Minimal interface using [id] and [compose]. *)
module type WITH_ID_AND_COMPOSE = sig
  type ('a, 'b) t
  (** The type held by the [Category]. *)

  include WITH_ID with type ('a, 'b) t := ('a, 'b) t

  include Semigroupoid.WITH_COMPOSE with type ('a, 'b) t := ('a, 'b) t
end

(** {1 Structure anatomy} *)

module type CORE = WITH_ID_AND_COMPOSE
(** Basis operations. *)

(** Additional operations. *)
module type OPERATION = sig
  type ('a, 'b) t
  (** The type held by the [Category]. *)

  include Semigroupoid.OPERATION with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** Infix operators. *)
module type INFIX = sig
  type ('a, 'b) t
  (** The type held by the [Category]. *)

  include Semigroupoid.INFIX with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of a [Category]. *)
module type API = sig
  (** {1 Type} *)

  type ('a, 'b) t
  (** The type held by the [Category]. *)

  (** {1 Functions} *)

  include CORE with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)

  include OPERATION with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type ('a, 'b) t = ('a, 'b) t

  include INFIX with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** {1 Additional references}

    - {{:https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Category.html}
      Haskell's documentation of Category} *)
