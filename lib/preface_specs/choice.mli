(** [Choice] is a [Profunctor] working on sum types (via [Either]). *)

(** Left operation. *)
module type WITH_LEFT = sig
  type ('a, 'b) t
  (** The type held by the [Choice Profunctor]. *)

  val left : ('a, 'b) t -> (('a, 'c) Either.t, ('b, 'c) Either.t) t
  (** Act on the left parameter of the sum. *)
end

(** Right operation. *)
module type WITH_RIGHT = sig
  type ('a, 'b) t
  (** The type held by the [Choice Profunctor]. *)

  val right : ('a, 'b) t -> (('c, 'a) Either.t, ('c, 'b) Either.t) t
  (** Act on the right parameter of the sum. *)
end

(** Requirement via [dimap] and [left]. *)
module type CORE_WITH_DIMAP_AND_LEFT = sig
  type ('a, 'b) t
  (** The type held by the [Choice Profunctor]. *)

  include Profunctor.CORE_WITH_DIMAP with type ('a, 'b) t := ('a, 'b) t

  include WITH_LEFT with type ('a, 'b) t := ('a, 'b) t
end

(** Requirement via [contramap_fst] and [map_snd] and [left]. *)
module type CORE_WITH_CONTRAMAP_FST_AND_MAP_SND_AND_LEFT = sig
  type ('a, 'b) t
  (** The type held by the [Choice Profunctor]. *)

  include
    Profunctor.CORE_WITH_CONTRAMAP_FST_AND_MAP_SND
      with type ('a, 'b) t := ('a, 'b) t

  include WITH_LEFT with type ('a, 'b) t := ('a, 'b) t
end

(** Requirement via [dimap] and [right]. *)
module type CORE_WITH_DIMAP_AND_RIGHT = sig
  type ('a, 'b) t
  (** The type held by the [Choice Profunctor]. *)

  include Profunctor.CORE_WITH_DIMAP with type ('a, 'b) t := ('a, 'b) t

  include WITH_RIGHT with type ('a, 'b) t := ('a, 'b) t
end

(** Requirement via [contramap_fst] and [map_snd] and [right]. *)
module type CORE_WITH_CONTRAMAP_FST_AND_MAP_SND_AND_RIGHT = sig
  type ('a, 'b) t
  (** The type held by the [Choice Profunctor]. *)

  include
    Profunctor.CORE_WITH_CONTRAMAP_FST_AND_MAP_SND
      with type ('a, 'b) t := ('a, 'b) t

  include WITH_RIGHT with type ('a, 'b) t := ('a, 'b) t
end

(** Standard requirement *)
module type CORE = sig
  type ('a, 'b) t
  (** The type held by the [Choice Profunctor]. *)

  include Profunctor.CORE with type ('a, 'b) t := ('a, 'b) t

  include WITH_LEFT with type ('a, 'b) t := ('a, 'b) t

  include WITH_RIGHT with type ('a, 'b) t := ('a, 'b) t
end

(** {1 API} *)

(** The complete interface of a [Choice Profunctor]. *)

module type API = CORE

(** {1 Bibliography}

    - {{:https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor.html#g:2}
      Haskell's documentation of Choice Profunctor} *)
