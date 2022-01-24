(** An [Applicative] is a functor with lifting and sequencing capabilities.
    [Applicative] is more general (and by extension weaker) than a
    {!module:Monad}. An [Applicative] is also a {!module:Functor}. *)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Applicative] must obey
    some laws.

    + [apply = lift2 id]
    + [lift2 f x y = f <$> x <*> y]
    + [pure id <*> v = v]
    + [pure (%) <*> u <*> v <*> w = u <*> (v <*> w)]
    + [pure f <*> pure x = pure (f x)]
    + [u <*> pure y = pure ((|>) y) <*> u]
    + [u *> v = (id <$ u) <*> v]
    + [u <* v = lift2 const u v]
    + [fmap f x = pure f <*> x]
    + [lift2 p (lift2 q u v) = lift2 f u % lift2 g v] *)

(** {1 Minimal definition} *)

(** Minimal interface using [map] and [product]. *)
module type WITH_PURE_MAP_AND_PRODUCT = sig
  type 'a t
  (** The type held by the [Applicative]. *)

  val pure : 'a -> 'a t
  (** Lift a value from ['a] into a new ['a t]. *)

  include Apply.WITH_MAP_AND_PRODUCT with type 'a t := 'a t
end

(** Minimal interface using [apply]. *)
module type WITH_PURE_AND_APPLY = sig
  type 'a t
  (** The type held by the [Applicative]. *)

  val pure : 'a -> 'a t
  (** Lift a value from ['a] into a new ['a t]. *)

  include Apply.WITH_APPLY with type 'a t := 'a t
end

(** Minimal interface using [lift2]. *)
module type WITH_PURE_AND_LIFT2 = sig
  type 'a t
  (** The type held by the [Applicative]. *)

  val pure : 'a -> 'a t
  (** Lift a value from ['a] into a new ['a t]. *)

  include Apply.WITH_LIFT2 with type 'a t := 'a t
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  include WITH_PURE_AND_APPLY
  (** @inline *)

  include WITH_PURE_MAP_AND_PRODUCT with type 'a t := 'a t
  (** @inline *)

  include WITH_PURE_AND_LIFT2 with type 'a t := 'a t
  (** @inline *)
end

(** Additional operations. *)
module type OPERATION = sig
  type 'a t
  (** The type held by the [Applicative]. *)

  include Apply.OPERATION with type 'a t := 'a t

  include Functor.OPERATION with type 'a t := 'a t
  (** @inline *)
end

(** Syntax extensions. *)
module type SYNTAX = sig
  type 'a t
  (** The type held by the [Applicative]. *)

  include Apply.SYNTAX with type 'a t := 'a t
end

(** Infix operators. *)
module type INFIX = sig
  type 'a t
  (** The type held by the [Applicative]. *)

  include Apply.INFIX with type 'a t := 'a t

  include Functor.INFIX with type 'a t := 'a t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of an [Applicative]. *)
module type API = sig
  (** {1 Type} *)

  type 'a t
  (** The type held by the [Applicative]. *)

  (** {1 Functions} *)

  include CORE with type 'a t := 'a t
  (** @inline *)

  include OPERATION with type 'a t := 'a t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type 'a t = 'a t

  include INFIX with type 'a t := 'a t
  (** @inline *)

  (** {1 Syntax} *)

  module Syntax : SYNTAX with type 'a t = 'a t

  include SYNTAX with type 'a t := 'a t
  (** @inline *)
end

(** {1 Additional references}

    - {{:http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html}
      Haskell's documentation of an Applicative Functor}
    - {{:http://www.staff.city.ac.uk/~ross/papers/Applicative.html} Applicative
      Programming with Effects} *)
