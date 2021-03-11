(** A [Profunctor] is a type constructor that takes two type arguments and is a
    [contravariant Functor] as first argument and a [covariant Functor] as
    second argument. *)

(** Requirement via [dimap]. *)
module type CORE_WITH_DIMAP = sig
  type ('a, 'b) t
  (** The type held by the [Profunctor]. *)

  val dimap : ('a -> 'b) -> ('c -> 'd) -> ('b, 'c) t -> ('a, 'd) t
end

(** Requirement via [contramap_fst] and [map_snd]. *)
module type CORE_WITH_CONTRAMAP_FST_AND_MAP_SND = sig
  type ('a, 'b) t
  (** The type held by the [Profunctor]. *)

  val contramap_fst : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
  (** Contramapping over the first argument. *)

  val map_snd : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Mapping over the second argument. *)
end

(** Standard requirement. *)
module type CORE = sig
  include CORE_WITH_DIMAP

  include CORE_WITH_CONTRAMAP_FST_AND_MAP_SND with type ('a, 'b) t := ('a, 'b) t
end

(** {1 API} *)

(** The complete interface of a [Profunctor]. *)

module type API = CORE

(** {1 Bibliography}

    - {{:https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor.html}
      Haskell's documentation of Profunctor} *)
