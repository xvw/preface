(** Automatization over category laws *)

open Aliases

module type LAWS = sig
  include Semigroupoid.LAWS

  val right_identity : string * (('a, 'b) t -> ('a, 'b) t pair)
  val left_identity : string * (('a, 'b) t -> ('a, 'b) t pair)
end

module Laws (C : Preface_specs.CATEGORY) :
  LAWS with type ('a, 'b) t = ('a, 'b) C.t
