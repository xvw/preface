open Aliases

module type LAWS = sig
  type ('a, 'b) t

  val associativity :
    string * (('a, 'b) t -> ('c, 'a) t -> ('d, 'c) t -> ('d, 'b) t pair)
end

module Laws (S : Preface_specs.SEMIGROUPOID) = struct
  type ('a, 'b) t = ('a, 'b) S.t

  let associativity =
    ( "f % (g % h) = (f % g) % h"
    , (fun f g h -> (S.(f % (g % h)), S.(f % g % h))) )
  ;;
end
