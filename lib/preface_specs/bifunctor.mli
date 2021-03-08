(** A [Bifunctor] is a type constructor that takes two type arguments and is a
    [Functor] in both arguments. *)

(** Requirement via [bimap]. *)
module type CORE_WITH_BIMAP = sig
  type ('a, 'b) t
  (** The type held by the [Bifunctor]*)

  val bimap : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
  (** Mapping over both arguments at the same time. *)
end

(** Requirement via [map_fst] and [map_snd]. *)
module type CORE_WITH_MAP_FST_AND_MAP_SND = sig
  type ('a, 'b) t
  (** The type held by the [Bifunctor]*)

  val map_fst : ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
  (** Mapping over the first argument. *)

  val map_snd : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Mapping over the second argument. *)
end

(** Standard requirement. *)
module type CORE = sig
  include CORE_WITH_BIMAP

  include CORE_WITH_MAP_FST_AND_MAP_SND with type ('a, 'b) t := ('a, 'b) t
end

(** Operations *)
module type OPERATION = sig
  type ('a, 'b) t
  (** The type held by the [Bifunctor]. *)

  val replace_fst : 'a -> ('b, 'c) t -> ('a, 'c) t
  (** Create a new [('a, 'b) t], replacing all values in the [('c, 'b) t] by
      given a value of ['a]. *)

  val replace_snd : 'a -> ('b, 'c) t -> ('b, 'a) t
  (** Create a new [('b, 'a) t], replacing all values in the [('b, 'c) t] by
      given a value of ['a]. *)
end

(** {1 API} *)

(** The complete interface of a [Bifunctor]. *)
module type API = sig
  include CORE

  include OPERATION with type ('a, 'b) t := ('a, 'b) t
end

(** {1 Bibliography}

    - {{:https://wiki.haskell.org/Typeclassopedia#Bifunctor} Bifunctor on
      Typeclassopedia} *)
