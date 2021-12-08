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
module type WITH_MAP_AND_PRODUCT = sig
  type 'a t
  (** The type held by the [Applicative]. *)

  val pure : 'a -> 'a t
  (** Lift a value from ['a] into a new ['a t]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Mapping over from ['a] to ['b] over ['a t] to ['b t]. *)

  val product : 'a t -> 'b t -> ('a * 'b) t
  (** Product functor mapping from ['a t] and ['b t] to [('a * 'b) t]. *)
end

(** Minimal interface using [apply]. *)
module type WITH_APPLY = sig
  type 'a t
  (** The type held by the [Applicative]. *)

  val pure : 'a -> 'a t
  (** Lift a value from ['a] into a new ['a t]. *)

  val apply : ('a -> 'b) t -> 'a t -> 'b t
  (** [Applicative] functor of [('a -> 'b) t] over ['a t] to ['b t]. *)
end

(** Minimal interface using [lift2]. *)
module type WITH_LIFT2 = sig
  type 'a t
  (** The type held by the [Applicative]. *)

  val pure : 'a -> 'a t
  (** Lift a value from ['a] into a new ['a t]. *)

  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Mapping over from ['a] and ['b] to ['c] over ['a t] and ['b t] to ['c t]. *)
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  include WITH_APPLY
  (** @inline *)

  include WITH_MAP_AND_PRODUCT with type 'a t := 'a t
  (** @inline *)

  include WITH_LIFT2 with type 'a t := 'a t
  (** @inline *)
end

(** Additional operations. *)
module type OPERATION = sig
  type 'a t
  (** The type held by the [Applicative]. *)

  val lift : ('a -> 'b) -> 'a t -> 'b t
  (** Mapping over from ['a] to ['b] over ['a t] to ['b t]. *)

  val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  (** Mapping over from ['a] and ['b] and ['c] to ['d] over ['a t] and ['b t]
      and ['c t] to ['d t]. *)

  include Functor.OPERATION with type 'a t := 'a t
  (** @inline *)
end

(** Syntax extensions. *)
module type SYNTAX = sig
  type 'a t
  (** The type held by the [Applicative]. *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  (** Flipped mapping over from ['a] to ['b] over ['a t] to ['b t]. *)

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  (** Product functor mapping from ['a t] and ['b t] to [('a * 'b) t]. *)
end

(** Infix operators. *)
module type INFIX = sig
  type 'a t
  (** The type held by the [Applicative]. *)

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  (** [Applicative] functor of [('a -> 'b) t] over ['a t] to ['b t]. *)

  val ( <**> ) : 'a t -> ('a -> 'b) t -> 'b t
  (** Flipped [Applicative] functor of [('a -> 'b) t] over ['a t] to ['b t]. *)

  val ( *> ) : unit t -> 'a t -> 'a t
  (** Discard the value of the first argument. *)

  val ( <* ) : 'a t -> unit t -> 'a t
  (** Discard the value of the second argument. *)

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
