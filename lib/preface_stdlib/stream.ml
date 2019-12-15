type 'a t = C of 'a * 'a t Lazy.t

let stream a l = C (a, l)

module Comonad = Preface_make.Comonad.Via_map_and_duplicate (struct
  type nonrec 'a t = 'a t

  let extract = function
    | C (a, _) -> a

  let rec duplicate = function
    | C (a, s) -> C (C (a, s), lazy (duplicate @@ Lazy.force s))

  let rec map f = function
    | C (a, s) -> C (f a, lazy (map f @@ Lazy.force s))
end)
