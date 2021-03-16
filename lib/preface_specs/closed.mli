(** [Closed] is a [Profunctor] working on exponential types (function). *)

(** Closed operation. *)
module type WITH_CLOSED = sig
  type ('a, 'b) t
  (** The type held by the [Closed Profunctor]. *)

  val closed : ('a, 'b) t -> ('c -> 'a, 'c -> 'b) t
  (** Act on the input type of a function. *)
end

(** Requirement via [dimap] and [closed]. *)
module type CORE_WITH_DIMAP_AND_CLOSED = sig
  type ('a, 'b) t
  (** The type held by the [Closed Profunctor]. *)

  include Profunctor.CORE_WITH_DIMAP with type ('a, 'b) t := ('a, 'b) t

  include WITH_CLOSED with type ('a, 'b) t := ('a, 'b) t
end

(** Requirement via [contramap_fst] and [map_snd] and [closed]. *)
module type CORE_WITH_CONTRAMAP_FST_AND_MAP_SND_AND_CLOSED = sig
  type ('a, 'b) t
  (** The type held by the [Closed Profunctor]. *)

  include
    Profunctor.CORE_WITH_CONTRAMAP_FST_AND_MAP_SND
      with type ('a, 'b) t := ('a, 'b) t

  include WITH_CLOSED with type ('a, 'b) t := ('a, 'b) t
end

(** Standard requirement *)
module type CORE = sig
  type ('a, 'b) t
  (** The type held by the [Closed Profunctor]. *)

  include Profunctor.CORE with type ('a, 'b) t := ('a, 'b) t

  include WITH_CLOSED with type ('a, 'b) t := ('a, 'b) t
end

(** Operation *)
module type OPERATION = sig
  type ('a, 'b) t
  (** The type held by the [Closed Profunctor]. *)

  val curry : ('a * 'b, 'c) t -> ('a, 'b -> 'c) t
  (** Transform an uncurried function into a curried one. *)
end

(** {1 API} *)

(** The complete interface of a [Closed Profunctor]. *)

module type API = sig
  include CORE

  include OPERATION with type ('a, 'b) t := ('a, 'b) t
end

(** {1 Bibliography}

    - {{:https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor.html#g:3}
      Haskell's documentation of Closed Profunctor} *)
