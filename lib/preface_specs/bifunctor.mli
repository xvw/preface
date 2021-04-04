(** A [Bifunctor] is a type constructor that takes two type arguments and is a
    {!module:Functor} (Covariant) in both arguments. *)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Bifunctor] must obey some
    laws.

    + [bimap id id = id]
    + [map_fst id = id]
    + [map_snd id = id]
    + [bimap f g = map_fst f % map_snd g]
    + [bimap  (f % g) (h % i) = bimap f h % bimap g i]
    + [map_fst  (f % g) = map_fst  f % map_snd  g]
    + [map_snd (f % g) = map_snd f % map_snd g]*)

(** {1 Structure anatomy} *)

(** Minimal interface using [bimap]. *)
module type CORE_WITH_BIMAP = sig
  type ('a, 'b) t
  (** The type held by the [Bifunctor]*)

  val bimap : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
  (** Mapping over both arguments at the same time. *)
end

(** Minimal interface using [map_fst] and [map_snd]. *)
module type CORE_WITH_MAP_FST_AND_MAP_SND = sig
  type ('a, 'b) t
  (** The type held by the [Bifunctor]*)

  val map_fst : ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
  (** Mapping over the first argument. *)

  val map_snd : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Mapping over the second argument. *)
end

(** The minimum definition of an [Bifunctor]. It is by using the combinators of
    this module that the other combinators will be derived. *)
module type CORE = sig
  include CORE_WITH_BIMAP
  (** @closed *)

  include CORE_WITH_MAP_FST_AND_MAP_SND with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** Additional operations. *)
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

(** {1 Complete API} *)

(** The complete interface of a [Bifunctor]. *)
module type API = sig
  (** {1 Core functions}

      Set of fundamental functions in the description of a [Bifunctor]. *)

  include CORE
  (** @closed *)

  (** {1 Additional functions}

      Additional functions, derived from fundamental functions. *)

  include OPERATION with type ('a, 'b) t := ('a, 'b) t
  (** @closed *)
end

(** {1 Additional references}

    - {{:https://wiki.haskell.org/Typeclassopedia#Bifunctor} Bifunctor on
      Typeclassopedia} *)
