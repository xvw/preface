type 'a t = 'a -> bool

let lift f = f

let run f x = f x

module Contravariant = Preface_make.Contravariant.Via_contramap (struct
  type nonrec 'a t = 'a t

  let contramap f g = Preface_core.Fun.(g % f)
end)
