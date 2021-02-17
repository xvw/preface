(** Automatization over arrow laws *)

module Laws (A : Preface_specs.ARROW) : sig
  val right_identity : string * (('a, 'b) A.t -> ('a, 'b) A.t * ('a, 'b) A.t)

  val left_identity : string * (('a, 'b) A.t -> ('a, 'b) A.t * ('a, 'b) A.t)

  val associativity :
    string
    * (   ('a, 'b) A.t
       -> ('c, 'a) A.t
       -> ('d, 'c) A.t
       -> ('d, 'b) A.t * ('d, 'b) A.t)

  val law1 : string * (unit -> ('a, 'a) A.t * ('a, 'a) A.t)

  val law2 : string * (('a -> 'b) -> ('b -> 'c) -> ('a, 'c) A.t * ('a, 'c) A.t)

  val law3 :
    string * (('a -> 'b) -> ('a * 'c, 'b * 'c) A.t * ('a * 'c, 'b * 'c) A.t)

  val law4 :
    string
    * (   ('a, 'b) A.t
       -> ('b, 'c) A.t
       -> ('a * 'd, 'c * 'd) A.t * ('a * 'd, 'c * 'd) A.t)

  val law5 :
    string
    * (   ('a, 'b) A.t
       -> ('c -> 'd)
       -> ('a * 'c, 'b * 'd) A.t * ('a * 'c, 'b * 'd) A.t)

  val law6 : string * (('a, 'b) A.t -> ('a * 'c, 'b) A.t * ('a * 'd, 'b) A.t)

  val law7 :
    string
    * (   ('a, 'b) A.t
       -> (('a * 'c) * 'd, 'b * ('c * 'd)) A.t
          * (('a * 'c) * 'd, 'b * ('c * 'd)) A.t)
end
