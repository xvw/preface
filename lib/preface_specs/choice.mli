(** [Choice] is a {!module:Profunctor} working on sum types (via
    {!module:Either}). *)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Strong] must obey some
    laws.

    + All {!module:Profunctor} laws
    + [left = dimap Either.swap Either.swap % right]
    + [right = dimap Either.swap Either.swap % left]
    + [map_snd Either.left = contramap_fst Either.left % left]
    + [map_snd Either.right = contramap_fst Either.right % right]
    + [contramap_fst (Fun.Choice.right f) % left = map_snd (Fun.Choice.right f) % left]
    + [contramap_fst (Fun.Choice.left f) % right = map_snd (Fun.Choice.left f) % right]
    + [left % left = dimap assoc unassoc % left]
    + [left % left = dimap unassoc assoc % right] *)

open Preface_core.Shims

(** {1 Minimal definition} *)

(** Minimal interface using [left] and without {!module:Profunctor}. *)
module type WITH_LEFT = sig
  type ('a, 'b) t
  (** The type held by the [Choice Profunctor]. *)

  val left : ('a, 'b) t -> (('a, 'c) Either.t, ('b, 'c) Either.t) t
  (** Act on the left parameter of the sum. *)
end

(** Minimal interface using [right] and without {!module:Profunctor}. *)
module type WITH_RIGHT = sig
  type ('a, 'b) t
  (** The type held by the [Choice Profunctor]. *)

  val right : ('a, 'b) t -> (('c, 'a) Either.t, ('c, 'b) Either.t) t
  (** Act on the right parameter of the sum. *)
end

(** Minimal interface [dimap] and [left]. *)
module type WITH_DIMAP_AND_LEFT = sig
  type ('a, 'b) t
  (** The type held by the [Choice Profunctor]. *)

  include Profunctor.WITH_DIMAP with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)

  include WITH_LEFT with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** Minimal interface using [contramap_fst] and [map_snd] and [left]. *)
module type WITH_CONTRAMAP_FST_AND_MAP_SND_AND_LEFT = sig
  type ('a, 'b) t
  (** The type held by the [Choice Profunctor]. *)

  include
    Profunctor.WITH_CONTRAMAP_FST_AND_MAP_SND with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)

  include WITH_LEFT with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** Minimal interface using [dimap] and [right]. *)
module type WITH_DIMAP_AND_RIGHT = sig
  type ('a, 'b) t
  (** The type held by the [Choice Profunctor]. *)

  include Profunctor.WITH_DIMAP with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)

  include WITH_RIGHT with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** Minimal interfaces using [contramap_fst] and [map_snd] and [right]. *)
module type WITH_CONTRAMAP_FST_AND_MAP_SND_AND_RIGHT = sig
  type ('a, 'b) t
  (** The type held by the [Choice Profunctor]. *)

  include
    Profunctor.WITH_CONTRAMAP_FST_AND_MAP_SND with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)

  include WITH_RIGHT with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  type ('a, 'b) t
  (** The type held by the [Choice Profunctor]. *)

  include Profunctor.CORE with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)

  include WITH_LEFT with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)

  include WITH_RIGHT with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of a [Choice Profunctor]. *)
module type API = sig
  (** {1 Type} *)

  type ('a, 'b) t
  (** The type held by the [Choice]. *)

  (** {1 Functions} *)

  include CORE with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** {1 Additional references}

    - {{:https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor.html#g:2}
      Haskell's documentation of Choice Profunctor} *)
