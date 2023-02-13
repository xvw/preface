(** A [Foldable] is a data structure which can be fold. In other word, reduced
    to a summary value one element at a time *)

(** {1 Minimal definition} *)

(** Minimal definition using [fold_map'].

    [fold_map' neutral combine f x] use explicit monoidal combinators passing in
    order to deal with polymorphism. *)
module type WITH_FOLD_MAP = sig
  type 'a t
  (** The type held by [Foldable]. *)

  include Indexed_foldable.WITH_FOLD_MAP with type ('a, _) t := 'a t
  (** @inline *)
end

(** Minimal definition using [fold_right].*)
module type WITH_FOLD_RIGHT = sig
  type 'a t
  (** The type held by [Foldable]. *)

  include Indexed_foldable.WITH_FOLD_RIGHT with type ('a, _) t := 'a t
  (** @inline *)
end

(** {1 Structure anatomy} *)

(** Basis operation. *)
module type CORE = sig
  type 'a t
  (** The type held by [Foldable]. *)

  include Indexed_foldable.CORE with type ('a, _) t := 'a t
  (** @inline *)
end

(** Additional operations. *)
module type OPERATION = sig
  type 'a t
  (** The type held by [Foldable]. *)

  include Indexed_foldable.OPERATION with type ('a, _) t := 'a t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of a [Foldable]. *)
module type API = sig
  (** {1 Type} *)

  type 'a t
  (** The type held by the [Foldable]. *)

  include Indexed_foldable.API with type ('a, _) t := 'a t
  (** @inline *)
end

(** {1 Additional references}

    - {{:https://wiki.haskell.org/Foldable_and_Traversable} Haskell's wiki of
      Foldable and Traversable}
    - {{:https://hackage.haskell.org/package/base-4.7.0.2/docs/Data-Foldable.html}
      Haskell's documentation of Foldable} *)
