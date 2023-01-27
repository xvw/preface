(** An [Apply] is a functor with lifting and sequencing capabilities. [Apply] is
    more general (and by extension weaker) than a {!module:Applicative}. An
    [Apply] is also a {!module:Functor}. *)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Apply] must obey some
    laws.

    + [apply = lift2 id]
    + [lift2 f x y = f <$> x <*> y]
    + [u *> v = (id <$ u) <*> v]
    + [u <* v = lift2 const u v]
    + [lift2 p (lift2 q u v) = lift2 f u % lift2 g v] *)

(** {1 Minimal definition} *)

(** Minimal interface using [product]. *)
module type WITH_PRODUCT = sig
  type 'a t
  (** The type held by the [Apply]. *)

  include Indexed_apply.WITH_PRODUCT with type ('a, _) t := 'a t
  (** @inline *)
end

(** Minimal interface using [apply]. *)
module type WITH_APPLY = sig
  type 'a t
  (** The type held by the [Apply]. *)

  include Indexed_apply.WITH_APPLY with type ('a, _) t := 'a t
  (** @inline *)
end

(** Minimal interface using [lift2]. *)
module type WITH_LIFT2 = sig
  type 'a t
  (** The type held by the [Apply]. *)

  include Indexed_apply.WITH_LIFT2 with type ('a, _) t := 'a t
  (** @inline *)
end

(** Minimal interface using [map] and [product]. *)
module type WITH_MAP_AND_PRODUCT = sig
  type 'a t
  (** The type held by the [Apply]. *)

  include Indexed_apply.WITH_MAP_AND_PRODUCT with type ('a, _) t := 'a t
  (** @inline *)
end

(** Minimal interface using [map] and [product]. *)
module type WITH_MAP_AND_APPLY = sig
  type 'a t
  (** The type held by the [Apply]. *)

  include Indexed_apply.WITH_MAP_AND_APPLY with type ('a, _) t := 'a t
  (** @inline *)
end

(** Minimal interface using [map] and [lift2]. *)
module type WITH_MAP_AND_LIFT2 = sig
  type 'a t
  (** The type held by the [Apply]. *)

  include Indexed_apply.WITH_MAP_AND_LIFT2 with type ('a, _) t := 'a t
  (** @inline *)
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  type 'a t
  (** The type held by the [Apply]. *)

  include Indexed_apply.CORE with type ('a, _) t := 'a t
  (** @inline *)
end

(** Additional operations. *)
module type OPERATION = sig
  type 'a t
  (** The type held by the [Apply]. *)

  include Indexed_apply.OPERATION with type ('a, _) t := 'a t
  (** @inline *)
end

(** Syntax extensions. *)
module type SYNTAX = sig
  type 'a t
  (** The type held by the [Apply]. *)

  include Indexed_apply.SYNTAX with type ('a, _) t := 'a t
  (** @inline *)
end

(** Infix operators. *)
module type INFIX = sig
  type 'a t
  (** The type held by the [Apply]. *)

  include Indexed_apply.INFIX with type ('a, _) t := 'a t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of an [Apply]. *)
module type API = sig
  (** {1 Type} *)

  type 'a t
  (** The type held by the [Apply]. *)

  (** {1 Functions} *)

  include CORE with type 'a t := 'a t
  (** @inline *)

  include OPERATION with type 'a t := 'a t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type 'a t = 'a t

  include module type of Infix with type 'a t := 'a t
  (** @inline *)

  (** {1 Syntax} *)

  module Syntax : SYNTAX with type 'a t = 'a t

  include module type of Syntax with type 'a t := 'a t
  (** @inline *)
end

(** {1 Additional references}

    - {{:https://hackage.haskell.org/package/semigroupoids-5.3.6/docs/Data-Functor-Apply.html#g:2}
      Haskell's documentation of an Apply Functor} *)
