open Aliases

module type LAWS = sig
  include Profunctor.LAWS

  val fst_define_snd : string * (('a, 'b) t -> ('a * 'c, 'b * 'c) t pair)

  val snd_define_fst : string * (('a, 'b) t -> ('c * 'a, 'c * 'b) t pair)

  val contramap_fst : string * (('a, 'b) t -> ('a * 'c, 'b) t pair)

  val contramap_snd : string * (('a, 'b) t -> ('c * 'a, 'b) t pair)

  val dinaturality_fst :
    string * (('a -> 'b) -> ('c, 'd) t -> ('c * 'a, 'd * 'b) t pair)

  val dinaturality_snd :
    string
    * (('a -> 'b) -> ('c, 'd) t -> ('a * 'c, 'b * 'd) t * ('a * 'c, 'b * 'd) t)

  val fst_fst_is_dmap :
    string * (('a, 'b) t -> (('a * 'c) * 'd, ('b * 'c) * 'd) t pair)

  val snd_snd_is_dmap :
    string
    * (   ('a * 'b, 'c * 'd) t
       -> ('e * ('f * ('a * 'b)), 'e * ('f * ('c * 'd))) t pair )
end

module Laws (S : Preface_specs.STRONG) :
  LAWS with type ('a, 'b) t = ('a, 'b) S.t
