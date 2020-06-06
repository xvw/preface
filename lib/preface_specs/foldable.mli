(** A [Foldable] is a data structure wich can be fold.

    {1 Structure anatomy} *)

(** Requirement via [fold_map'].

    [fold_map' neutral combine f x] use explicit monoidal combinators passing in
    order to deal with polymorphsim. *)
module type CORE_WITH_FOLD_MAP = sig
  type 'a t
  (** The type held by [Foldable]. *)

  val fold_map' : 'a -> ('a -> 'a -> 'a) -> ('b -> 'a) -> 'b t -> 'a
  (** Map each element of the [foldable] to a [monoid] an combine the result. *)
end

(** Requirement via [fold_right].*)
module type CORE_WITH_FOLD_RIGHT = sig
  type 'a t
  (** The type held by [Foldable]. *)

  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** Same of {!val:List.fold_right} for [Foldable]. *)
end

(** Standard requirement. *)
module type CORE = sig
  include CORE_WITH_FOLD_MAP

  include CORE_WITH_FOLD_RIGHT with type 'a t := 'a t
end

(** Operations. *)
module type OPERATION = sig
  type 'a t
  (** The type held by [Foldable]. *)

  val reduce : (module Monoid.CORE with type t = 'm) -> 'm t -> 'm
  (** Reduce a [Foldable] using [combine] of the given monoid. *)

  val fold_map :
    (module Monoid.CORE with type t = 'm) -> ('a -> 'm) -> 'a t -> 'm
  (** Same of {!val:CORE.fold_map'} but using a [monoid] module instead of
      giving explicitely [neutral] and [combine].*)

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** Same of {!val:List.fold_left} for [Foldable]. *)

  val for_all : ('a -> bool) -> 'a t -> bool
  (** Checks if all elements of the [foldable] satisfy the given predicate. *)

  val exists : ('a -> bool) -> 'a t -> bool
  (** Checks if at least on element of the [foldable] satisfy the given
      predicate. *)

  val length : 'a t -> int
  (** Count the number of elements in the [foldable]. *)
end

(** {1 API} *)

(** The complete interface of a [Foldable]. *)
module type API = sig
  include CORE

  include OPERATION with type 'a t := 'a t
end

(** {1 Bibliography}

    - {{:https://wiki.haskell.org/Foldable_and_Traversable} Haskell's wiki of
      Foldable and Traversable}
    - {{:https://hackage.haskell.org/package/base-4.7.0.2/docs/Data-Foldable.html}
      Haskell's documentation of Foldable} *)
