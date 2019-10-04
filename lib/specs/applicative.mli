(** An [Applicative] for [('a -> 'b) t] is an application functor
    from ['a t] to ['b t].

    {2 Laws of [Applicative]}

    To have a predictable behaviour, the instance of [Applicative] must
    obey some laws.

    - [apply (pure id)] must be equivalent to [id];
    - [compose <$> u <*> v <*> w] must be equivalent to
      [u <*> v <*> w];
    - [f <$> pure x] must be equivalent to [pure (f x)];
    - [u <*> pure x] must be equivalent to [(fun f -> f x) <$> u];
*)

(** {1 Structure anatomy} *)

(** Requirement via [map] and [product]. *)
module type CORE_VIA_MAP_AND_PRODUCT = sig
  type 'a t
  (** The type holded by the [Applicative]. *)

  val pure : 'a -> 'a t
  (** Create a new ['a t]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Mapping over from ['a] to ['b] over ['a t] to ['b t]. *)

  val product : 'a t -> 'b t -> ('a * 'b) t
  (** Product functor mapping from ['a t] and ['b t] to [('a * 'b) t]. *)
end

(** Requirement via [apply]. *)
module type CORE_VIA_APPLY = sig
  type 'a t
  (** The type holded by the [Applicative]. *)

  val pure : 'a -> 'a t
  (** Create a new ['a t]. *)

  val apply : ('a -> 'b) t -> 'a t -> 'b t
  (** Applicative functor of [('a -> 'b) t] over ['a t] to ['b t]. *)
end

(** Standard requirement. *)
module type CORE = sig
  include CORE_VIA_APPLY

  include CORE_VIA_MAP_AND_PRODUCT with type 'a t := 'a t
end

(** Operations *)
module type OPERATION = sig
  type 'a t
  (** The type holded by the [Applicative]. *)

  val lift : ('a -> 'b) -> 'a t -> 'b t
  (** Mapping over from ['a] to ['b] over ['a t] to ['b t]. *)

  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Mapping over from ['a] and ['b] to ['c] over ['a t] and
      ['b t] to ['c t].
  *)

  val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  (** Mapping over from ['a] and ['b] and ['c] to ['d] over ['a t]
      and ['b t] and ['c t] to ['d t].
  *)
end

(** Syntax extensions *)
module type SYNTAX = sig
  type 'a t
  (** The type holded by the [Applicative]. *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  (** Flipped mapping over from ['a] to ['b] over ['a t] to ['b t]. *)

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  (** Product functor mapping from ['a t] and ['b t] to [('a * 'b) t]. *)
end

(** Infix notations *)
module type INFIX = sig
  type 'a t
  (** The type holded by the [Applicative]. *)

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  (** Applicative functor of [('a -> 'b) t] over ['a t] to ['b t]. *)

  val ( <**> ) : 'a t -> ('a -> 'b) t -> 'b t
  (** Flipped applicative functor of [('a -> 'b) t] over ['a t] to ['b t]. *)

  val ( *> ) : 'a t -> 'b t -> 'b t
  (** Discard the value of the first argument. *)

  val ( <* ) : 'a t -> 'b t -> 'a t
  (** Discard the value of the second argument. *)
end

(** {1 API} *)

(** The complete interface of an [Applicative]. *)
module type API = sig
  include CORE

  include OPERATION with type 'a t := 'a t

  module Syntax : SYNTAX with type 'a t := 'a t

  include module type of Syntax

  module Infix : INFIX with type 'a t := 'a t

  include module type of Infix
end

(** 
   {1 Bibliography}

   - {{: http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html} 
   Haskell's documentation of an Applicative Functor}
   - {{: http://www.staff.city.ac.uk/~ross/papers/Applicative.html} 
   Applicative Programming with Effects}
*)
