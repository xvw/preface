open Aliases

module type LAWS = sig
  type ('a, 'b) t

  val associativity :
    string * (('a, 'b) t -> ('c, 'a) t -> ('d, 'c) t -> ('d, 'b) t pair)
end

module Laws (C : Preface_specs.SEMIGROUPOID) = struct
  type ('a, 'b) t = ('a, 'b) C.t

  let associativity =
    ( "f % (g % h) = (f % g) % h"
    , (fun f g h -> (C.(f % (g % h)), C.(f % g % h))) )
  ;;
end
