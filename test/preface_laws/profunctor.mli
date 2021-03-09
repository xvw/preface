(** Automatization over profunctor laws *)

open Aliases

module type LAWS = sig
  type ('a, 'b) t

  val dimap_identity : string * (('a, 'b) t -> ('a, 'b) t pair)

  val contramap_fst_identity : string * (('a, 'b) t -> ('a, 'b) t pair)

  val map_snd_identity : string * (('a, 'b) t -> ('a, 'b) t pair)

  val dimap_equality :
    string * (('a -> 'b) -> ('c -> 'd) -> ('b, 'c) t -> ('a, 'd) t pair)

  val dimap_parametricity :
    string
    * (   ('a -> 'b)
       -> ('c -> 'a)
       -> ('d -> 'e)
       -> ('f -> 'd)
       -> ('b, 'f) t
       -> ('c, 'e) t pair )

  val contramap_fst_parametricity :
    string * (('a -> 'b) -> ('c -> 'a) -> ('b, 'd) t -> ('c, 'd) t pair)

  val map_snd_parametricity :
    string * (('a -> 'b) -> ('c -> 'a) -> ('d, 'c) t -> ('d, 'b) t pair)
end

module Laws (P : Preface_specs.PROFUNCTOR) :
  LAWS with type ('a, 'b) t = ('a, 'b) P.t
