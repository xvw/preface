(** Automatization over category laws *)

open Aliases

module type LAWS = sig
  type ('a, 'b) t

  val right_identity : string * (('a, 'b) t -> ('a, 'b) t pair)

  val left_identity : string * (('a, 'b) t -> ('a, 'b) t pair)

  val associativity :
    string * (('a, 'b) t -> ('c, 'a) t -> ('d, 'c) t -> ('d, 'b) t pair)
end

module Laws (C : Preface_specs.CATEGORY) :
  LAWS with type ('a, 'b) t = ('a, 'b) C.t
