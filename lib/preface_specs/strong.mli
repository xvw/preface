(** [Strong] is a [Profunctor] working on product types. *)

(** Fst operation. *)
module type WITH_FST = sig
  type ('a, 'b) t
  (** The type held by the [Strong Profunctor]. *)

  val fst : ('a, 'b) t -> ('a * 'c, 'b * 'c) t
  (** Act on the first parameter of the product. *)
end

(** Snd operation. *)
module type WITH_SND = sig
  type ('a, 'b) t
  (** The type held by the [Strong Profunctor]. *)

  val snd : ('b, 'c) t -> ('a * 'b, 'a * 'c) t
  (** Act on the second parameter of the product. *)
end

(** Requirement via [dimap] and [fst]. *)
module type CORE_WITH_DIMAP_AND_FST = sig
  type ('a, 'b) t
  (** The type held by the [Strong Profunctor]. *)

  include Profunctor.CORE_WITH_DIMAP with type ('a, 'b) t := ('a, 'b) t

  include WITH_FST with type ('a, 'b) t := ('a, 'b) t
end

(** Requirement via [contramap_fst] and [map_snd] and [fst]. *)
module type CORE_WITH_CONTRAMAP_FST_AND_MAP_SND_AND_FST = sig
  type ('a, 'b) t
  (** The type held by the [Strong Profunctor]. *)

  include
    Profunctor.CORE_WITH_CONTRAMAP_FST_AND_MAP_SND
      with type ('a, 'b) t := ('a, 'b) t

  include WITH_FST with type ('a, 'b) t := ('a, 'b) t
end

(** Requirement via [dimap] and [snd]. *)
module type CORE_WITH_DIMAP_AND_SND = sig
  type ('a, 'b) t
  (** The type held by the [Strong Profunctor]. *)

  include Profunctor.CORE_WITH_DIMAP with type ('a, 'b) t := ('a, 'b) t

  include WITH_SND with type ('a, 'b) t := ('a, 'b) t
end

(** Requirement via [contramap_fst] and [map_snd] and [snd]. *)
module type CORE_WITH_CONTRAMAP_FST_AND_MAP_SND_AND_SND = sig
  type ('a, 'b) t
  (** The type held by the [Strong Profunctor]. *)

  include
    Profunctor.CORE_WITH_CONTRAMAP_FST_AND_MAP_SND
      with type ('a, 'b) t := ('a, 'b) t

  include WITH_SND with type ('a, 'b) t := ('a, 'b) t
end

(** Standard requirement *)
module type CORE = sig
  type ('a, 'b) t
  (** The type held by the [Strong Profunctor]. *)

  include Profunctor.CORE with type ('a, 'b) t := ('a, 'b) t

  include WITH_FST with type ('a, 'b) t := ('a, 'b) t

  include WITH_SND with type ('a, 'b) t := ('a, 'b) t
end

(** Operations. *)
module type OPERATION = sig
  type ('a, 'b) t
  (** The type held by the [Strong Profunctor]. *)

  val uncurry : ('a, 'b -> 'c) t -> ('a * 'b, 'c) t
  (** Uncurry a product strong profunctor. *)

  val strong : ('a -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Lift a function into a strong profunctor. *)
end

(** {1 API} *)

(** The complete interface of a [Strong Profunctor]. *)

module type API = sig
  include CORE

  include OPERATION with type ('a, 'b) t := ('a, 'b) t
end

(** {1 Bibliography}

    - {{:https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor.html#g:2}
      Haskell's documentation of Strong Profunctor}
    - {{:http://www.riec.tohoku.ac.jp/~asada/papers/arrStrMnd.pdf} Arrows are
      Strong Monads} *)
