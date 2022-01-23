type ('a, 'b) t = 'a * 'b

let fst (x, _) = x
let snd (_, x) = x
let swap (x, y) = (y, x)
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y
let equal f g (x, y) (x', y') = f x x' && g y y'
let pp f g formater (x, y) = Format.fprintf formater "(%a, %a)" f x g y

module Infix = struct
  let ( & ) x y = (x, y)
end

include Infix

module Bifunctor = Preface_make.Bifunctor.Via_bimap (struct
  type nonrec ('a, 'b) t = ('a, 'b) t

  let bimap f g (x, y) = (f x, g y)
end)
