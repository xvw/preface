(** Automatization over category laws *)

module Laws (C : Preface_specs.CATEGORY) : sig
  val right_identity : string * (('a, 'b) C.t -> ('a, 'b) C.t * ('a, 'b) C.t)

  val left_identity : string * (('a, 'b) C.t -> ('a, 'b) C.t * ('a, 'b) C.t)

  val associativity :
    string
    * (   ('a, 'b) C.t
       -> ('c, 'a) C.t
       -> ('d, 'c) C.t
       -> ('d, 'b) C.t * ('d, 'b) C.t)
end
