(** Requirement via [bimap]. *)
module type CORE_WITH_BIMAP = sig
  type ('a, 'b) t
  (** The type held by the [Bifunctor]*)

  val bimap : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
  (** Mapping over both arguments at the same time. *)
end

(** Requirement via [fst] and [snd]. *)
module type CORE_WITH_FST_AND_SND = sig
  type ('a, 'b) t
  (** The type held by the [Bifunctor]*)

  val fst : ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
  (** Mapping over the first argument. *)

  val snd : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Mapping over the second argument. *)
end

(** Standard requirement. *)
module type CORE = sig
  include CORE_WITH_BIMAP

  include CORE_WITH_FST_AND_SND with type ('a, 'b) t := ('a, 'b) t
end

(** Operations *)
module type OPERATION = sig
  type ('a, 'b) t
  (** The type held by the [Bifunctor]. *)

  val map : ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
  (** Mapping over from ['a] to ['b] over [('a, 'c) t] to [('b, 'c) t]. *)

  val replace_fst : 'a -> ('b, 'c) t -> ('a, 'c) t
  (** Create a new [('a, 'b) t], replacing all values in the [('c, 'b) t] by
      given a value of ['a]. *)

  val replace_snd : 'a -> ('b, 'c) t -> ('b, 'a) t
  (** Create a new [('b, 'a) t], replacing all values in the [('b, 'c) t] by
      given a value of ['a]. *)
end

(** Infix notation *)
module type INFIX = sig
  type ('a, 'b) t
  (** The type held by the [Bifunctor]. *)

  val ( <$> ) : ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
  (** Infix version of {!val:OPERATION.map}. *)

  val ( <&> ) : ('a, 'c) t -> ('a -> 'b) -> ('b, 'c) t
  (** Flipped and infix version of {!val:OPERATION.map}. *)
end

(** {1 API} *)

(** The complete interface of a [Bifunctor]. *)
module type API = sig
  include CORE

  include OPERATION with type ('a, 'b) t := ('a, 'b) t

  module Infix : INFIX with type ('a, 'b) t := ('a, 'b) t

  include module type of Infix
end

(** {1 Bibliography}

    - {{:https://wiki.haskell.org/Typeclassopedia#Bifunctor} Bifunctor on
      Typeclassopedia} *)
