open Aliases

module type LAWS = sig
  type ('a, 'b) t

  val right_identity : string * (('a, 'b) t -> ('a, 'b) t pair)

  val left_identity : string * (('a, 'b) t -> ('a, 'b) t pair)

  val associativity :
    string * (('a, 'b) t -> ('c, 'a) t -> ('d, 'c) t -> ('d, 'b) t pair)
end

module Laws (C : Preface_specs.CATEGORY) = struct
  type ('a, 'b) t = ('a, 'b) C.t

  let right_identity = ("f % id = f", (fun f -> (C.(f % id), f)))

  let left_identity = ("id % f = f", (fun f -> (C.(id % f), f)))

  let associativity =
    ( "f % (g % h) = (f % g) % h"
    , (fun f g h -> (C.(f % (g % h)), C.(f % g % h))) )
  ;;
end
