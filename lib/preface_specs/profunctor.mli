(** A [Profunctor] is a type constructor that takes two type arguments and is a
    {!module:Contravariant} [Functor] as first argument and a [covariant]
    {!module:Functor} as second argument. *)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Bifunctor] must obey some
    laws.

    + [dimap id id = id]
    + [contramap_fst id = id]
    + [map_snd id = id]
    + [dimap f g = contramap_fst f % map_snd g]
    + [dimap (f % g) (h % i) = dimap g h % dimap f i]
    + [contramap_fst (f % g) = contramap_fst g % contramap_fst f]
    + [map_snd (f % g) = map_snd f % map_snd g] *)

(** {1 Minimal definition} *)

(** Minimal interface using [dimap]. *)
module type WITH_DIMAP = sig
  type ('a, 'b) t
  (** The type held by the [Profunctor]. *)

  val dimap : ('a -> 'b) -> ('c -> 'd) -> ('b, 'c) t -> ('a, 'd) t
end

(** Minimal interface using [contramap_fst] and [map_snd]. *)
module type WITH_CONTRAMAP_FST_AND_MAP_SND = sig
  type ('a, 'b) t
  (** The type held by the [Profunctor]. *)

  val contramap_fst : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
  (** Contramapping over the first argument. *)

  val map_snd : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Mapping over the second argument. *)
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  include WITH_DIMAP
  (** @closed *)

  include WITH_CONTRAMAP_FST_AND_MAP_SND with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** {1 Complete API} *)

module type API = CORE
(** The complete interface of a [Profunctor]. *)

(** {1 Additional references}

    - {{:https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor.html}
      Haskell's documentation of Profunctor} *)
