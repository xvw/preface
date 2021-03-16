open Aliases

module type LAWS = sig
  include Profunctor.LAWS

  val contramap_fst_closed :
    string * (('a -> 'b) -> ('c, 'd) t -> ('b -> 'c, 'a -> 'd) t pair)

  val closed_closed :
    string * (('a, 'b) t -> ('c -> 'd -> 'a, 'c -> 'd -> 'b) t pair)

  val dimap_const : string * (('a, 'b) t -> ('a, 'b) t pair)
end

module Laws (C : Preface_specs.CLOSED) :
  LAWS with type ('a, 'b) t = ('a, 'b) C.t
