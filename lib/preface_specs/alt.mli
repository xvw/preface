(** [Alt] is a {!module:Functor} which is a kind of {!module:Semigroup} over a
    parametrized type. In other word, [Alt] is a {!module:Functor} with a
    [combine] operation. *)

(** {2 Laws}

    To ensure that the derived combiners work properly, an [Alt] should respect
    these laws:

    + [combine (combine a b) c = combine a (combine b c)]
    + [map f (combine a b) = combine (map f a) (map f b)] *)

(** {1 Minimal definition} *)

(** Combine operation. This signature is mainly used to enrich a
    {!module:Functor} with [combine].*)
module type WITH_COMBINE = sig
  type 'a t
  (** A type ['a t] held by the [Alt]. *)

  include Indexed_alt.WITH_COMBINE with type ('a, _) t := 'a t
  (** @inline *)
end

(** The minimum definition of an [Alt]. It is by using the combinators of this
    module that the other combinators will be derived. *)
module type WITH_COMBINE_AND_MAP = sig
  type 'a t
  (** A type ['a t] held by the [Alt]. *)

  include Indexed_alt.WITH_COMBINE_AND_MAP with type ('a, _) t := 'a t
  (** @inline *)
end

(** {1 Structure anatomy} *)

module type CORE = WITH_COMBINE_AND_MAP
(** Basis operations.*)

(** Additional operations. *)
module type OPERATION = sig
  type 'a t
  (** A type ['a t] held by the [Alt]. *)

  include Indexed_alt.OPERATION with type ('a, _) t := 'a t
  (** @inline *)
end

(** Infix operators. *)
module type INFIX = sig
  type 'a t
  (** A type ['a t] which is an [Alt]. *)

  include Indexed_alt.INFIX with type ('a, _) t := 'a t
  (** @inline *)
end

(** Syntax operators. *)
module type SYNTAX = sig
  type 'a t
  (** A type ['a t] which is an [Alt]. *)

  include Indexed_alt.SYNTAX with type ('a, _) t := 'a t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of an [Alt]. *)
module type API = sig
  (** {1 Type} *)

  type 'a t
  (** The type held by the [Alt]. *)

  include Indexed_alt.API with type ('a, _) t := 'a t
  (** @inline *)
end

(** {1 Additional references}

    - {{:https://hackage.haskell.org/package/semigroupoids-5.3.4/docs/Data-Functor-Alt.html}
      Haskell's documentation of Alt} *)
