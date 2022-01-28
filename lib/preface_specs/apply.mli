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
  (** The type held by the [Applicative]. *)

  val product : 'a t -> 'b t -> ('a * 'b) t
  (** Product functor mapping from ['a t] and ['b t] to [('a * 'b) t]. *)
end

(** Minimal interface using [apply]. *)
module type WITH_APPLY = sig
  type 'a t
  (** The type held by the [Applicative]. *)

  val apply : ('a -> 'b) t -> 'a t -> 'b t
  (** [Applicative] functor of [('a -> 'b) t] over ['a t] to ['b t]. *)
end

(** Minimal interface using [lift2]. *)
module type WITH_LIFT2 = sig
  type 'a t
  (** The type held by the [Applicative]. *)

  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Mapping over from ['a] and ['b] to ['c] over ['a t] and ['b t] to ['c t]. *)
end

(** Minimal interface using [map] and [product]. *)
module type WITH_MAP_AND_PRODUCT = sig
  type 'a t
  (** The type held by the [Applicative]. *)

  include Functor.WITH_MAP with type 'a t := 'a t
  include WITH_PRODUCT with type 'a t := 'a t
end

(** Minimal interface using [map] and [product]. *)
module type WITH_MAP_AND_APPLY = sig
  type 'a t
  (** The type held by the [Applicative]. *)

  include Functor.WITH_MAP with type 'a t := 'a t
  include WITH_APPLY with type 'a t := 'a t
end

(** Minimal interface using [map] and [lift2]. *)
module type WITH_MAP_AND_LIFT2 = sig
  type 'a t
  (** The type held by the [Applicative]. *)

  include Functor.WITH_MAP with type 'a t := 'a t
  include WITH_LIFT2 with type 'a t := 'a t
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  include Functor.WITH_MAP
  (** @inline *)

  include WITH_APPLY with type 'a t := 'a t
  (** @inline *)

  include WITH_PRODUCT with type 'a t := 'a t
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

  module Infix : INFIX with type 'a t := 'a t

  include module type of Infix
  (** @inline *)

  (** {1 Syntax} *)

  module Syntax : SYNTAX with type 'a t := 'a t

  include module type of Syntax
  (** @inline *)
end

(** {1 Additional references}

    - {{:https://hackage.haskell.org/package/semigroupoids-5.3.6/docs/Data-Functor-Apply.html#g:2}
      Haskell's documentation of an Apply Functor} *)
