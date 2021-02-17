module Laws (C : Preface_specs.CATEGORY) = struct
  let right_identity = ("f % id = f", (fun f -> (C.(f % id), f)))

  let left_identity = ("id % f = f", (fun f -> (C.(id % f), f)))

  let associativity =
    ( "f % (g % h) = (f % g) % h"
    , (fun f g h -> (C.(f % (g % h)), C.(f % g % h))) )
  ;;
end
