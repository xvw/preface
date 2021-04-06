(** [Closed] is a {!module:Profunctor} working on exponential types (function). *)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Closed] must obey some
    laws.

    + All {!module:Profunctor} laws
    + [contramap_fst (fun x -> x % f) % closed = map_snd (fun x -> x % f) % closed]
    + [closed % closed = dimap uncurry curry % closed]
    + [dimap const (fun f -> f ()) % closed = id] *)

(** {1 Minimal definition} *)

(** Minimum interface using [closed] and without {!module:Profunctor}
    requirements. *)
module type WITH_CLOSED = sig
  type ('a, 'b) t
  (** The type held by the [Closed Profunctor]. *)

  val closed : ('a, 'b) t -> ('c -> 'a, 'c -> 'b) t
  (** Act on the input type of a function. *)
end

(** Minimum interface using [dimap] and [closed]. *)
module type WITH_DIMAP_AND_CLOSED = sig
  type ('a, 'b) t
  (** The type held by the [Closed Profunctor]. *)

  include Profunctor.WITH_DIMAP with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)

  include WITH_CLOSED with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** Minimum interface using [contramap_fst] and [map_snd] and [closed]. *)
module type WITH_CONTRAMAP_FST_AND_MAP_SND_AND_CLOSED = sig
  type ('a, 'b) t
  (** The type held by the [Closed Profunctor]. *)

  include
    Profunctor.WITH_CONTRAMAP_FST_AND_MAP_SND with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)

  include WITH_CLOSED with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  type ('a, 'b) t
  (** The type held by the [Closed Profunctor]. *)

  include Profunctor.CORE with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)

  include WITH_CLOSED with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** Additional operations. *)
module type OPERATION = sig
  type ('a, 'b) t
  (** The type held by the [Closed Profunctor]. *)

  val curry : ('a * 'b, 'c) t -> ('a, 'b -> 'c) t
  (** Transform an uncurried function into a curried one. *)
end

(** {1 Complete API} *)

(** The complete interface of a [Closed Profunctor]. *)
module type API = sig
  (** {1 Core functions}

      Set of fundamental functions in the description of a [Closed]. *)

  include CORE
  (** @closed *)

  (** {1 Additional functions}

      Additional functions, derived from fundamental functions. *)

  include OPERATION with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** {1 Additional references}

    - {{:https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor.html#g:3}
      Haskell's documentation of Closed Profunctor} *)
