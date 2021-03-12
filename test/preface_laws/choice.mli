open Aliases
open Preface_core.Shims

module type LAWS = sig
  include Profunctor.LAWS

  val left_defined_by_right :
    string * (('a, 'b) t -> (('a, 'c) Either.t, ('b, 'c) Either.t) t pair)

  val right_defined_by_left :
    string * (('a, 'b) t -> (('c, 'a) Either.t, ('c, 'b) Either.t) t pair)

  val map_snd_left : string * (('a, 'b) t -> ('a, ('b, 'c) Either.t) t pair)

  val map_snd_right : string * (('a, 'b) t -> ('a, ('c, 'b) Either.t) t pair)

  val contramap_fst_right :
    string
    * (('a -> 'b) -> ('c, 'd) t -> (('c, 'a) Either.t, ('d, 'b) Either.t) t pair)

  val contramap_fst_left :
    string
    * (('a -> 'b) -> ('c, 'd) t -> (('a, 'c) Either.t, ('b, 'd) Either.t) t pair)

  val left_left :
    string
    * (   ('a, 'b) t
       -> ((('a, 'c) Either.t, 'd) Either.t, (('b, 'c) Either.t, 'd) Either.t) t
          pair )

  val right_right :
    string
    * (   ('a, 'b) t
       -> (('c, ('d, 'a) Either.t) Either.t, ('c, ('d, 'b) Either.t) Either.t) t
          pair )
end

module Laws (C : Preface_specs.CHOICE) :
  LAWS with type ('a, 'b) t = ('a, 'b) C.t
