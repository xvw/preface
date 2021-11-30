(** Automatization over category laws *)

open Aliases

module type LAWS = sig
  type ('a, 'b) t

  val associativity :
    string * (('a, 'b) t -> ('c, 'a) t -> ('d, 'c) t -> ('d, 'b) t pair)
end

module Laws (C : Preface_specs.SEMIGROUPOID) :
  LAWS with type ('a, 'b) t = ('a, 'b) C.t
