(** [Strong] is a {!module:Profunctor} working on product types. *)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Strong] must obey some
    laws.

    + All {!module:Profunctor} laws
    + [fst = dimap Pair.swap Pair.swap % snd]
    + [contramap_fst Pair.fst = map_snd Pair.fst % fst]
    + [contramap_fst (Fun.snd f) % fst = map_snd (Fun.snd f) % fst]
    + [fst % fst = dimap (fun ((a,b),c) -> (a,(b,c))) (fun (a,(b,c)) -> ((a,b),c)) % fst]
    + [snd = dimap Pair.swap Pair.swap % fst]
    + [contramap_fst Pair.snd = map_snd Pair.snd % snd]
    + [contramap_fst (Fun.fst f) % snd = map_snd (Fun.fst f) % snd]
    + [snd % snd = dimap  (fun (a,(b,c)) -> ((a,b),c))  (fun ((a,b),c) -> (a,(b,c))) % snd ]*)

(** {1 Minimal definition} *)

(** Minimal interface using with [fst] and without {!module:Profunctor}
    requirements. *)
module type WITH_FST = sig
  type ('a, 'b) t
  (** The type held by the [Strong Profunctor]. *)

  val fst : ('a, 'b) t -> ('a * 'c, 'b * 'c) t
  (** Act on the first parameter of the product. *)
end

(** Minimal interface using with [snd] and without {!module:Profunctor}
    requirements. *)
module type WITH_SND = sig
  type ('a, 'b) t
  (** The type held by the [Strong Profunctor]. *)

  val snd : ('b, 'c) t -> ('a * 'b, 'a * 'c) t
  (** Act on the second parameter of the product. *)
end

(** Minimal interface using [dimap] and [fst]. *)
module type WITH_DIMAP_AND_FST = sig
  type ('a, 'b) t
  (** The type held by the [Strong Profunctor]. *)

  include Profunctor.WITH_DIMAP with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)

  include WITH_FST with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** Minimal interface using [contramap_fst] and [map_snd] and [fst]. *)
module type WITH_CONTRAMAP_FST_AND_MAP_SND_AND_FST = sig
  type ('a, 'b) t
  (** The type held by the [Strong Profunctor]. *)

  include
    Profunctor.WITH_CONTRAMAP_FST_AND_MAP_SND with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)

  include WITH_FST with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** Minimal interface using [dimap] and [snd]. *)
module type WITH_DIMAP_AND_SND = sig
  type ('a, 'b) t
  (** The type held by the [Strong Profunctor]. *)

  include Profunctor.WITH_DIMAP with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)

  include WITH_SND with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** Minimal interface using [contramap_fst] and [map_snd] and [snd]. *)
module type WITH_CONTRAMAP_FST_AND_MAP_SND_AND_SND = sig
  type ('a, 'b) t
  (** The type held by the [Strong Profunctor]. *)

  include
    Profunctor.WITH_CONTRAMAP_FST_AND_MAP_SND with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)

  include WITH_SND with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  type ('a, 'b) t
  (** The type held by the [Strong Profunctor]. *)

  include Profunctor.CORE with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)

  include WITH_FST with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)

  include WITH_SND with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** Additional operations. *)
module type OPERATION = sig
  type ('a, 'b) t
  (** The type held by the [Strong Profunctor]. *)

  val uncurry : ('a, 'b -> 'c) t -> ('a * 'b, 'c) t
  (** Uncurry a product [Strong Profunctor]. *)

  val strong : ('a -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Lift a function into a [Strong Profunctor]. *)
end

(** {1 Complete API} *)

(** The complete interface of a [Strong Profunctor]. *)
module type API = sig
  (** {1 Core functions}

      Set of fundamental functions in the description of a [Strong]. *)

  include CORE
  (** @closed *)

  (** {1 Additional functions}

      Additional functions, derived from fundamental functions. *)

  include OPERATION with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** {1 Additional references}

    - {{:https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor.html#g:2}
      Haskell's documentation of Strong Profunctor}
    - {{:http://www.riec.tohoku.ac.jp/~asada/papers/arrStrMnd.pdf} Arrows are
      Strong Monads} *)
