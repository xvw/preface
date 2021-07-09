module Endo (T : Preface_specs.Types.T0) =
Preface_make.Monoid.Via_combine_and_neutral (struct
  open Preface_core.Fun.Infix

  type t = T.t -> T.t

  let neutral x = x
  let combine f g = f % g
end)

module Dual (T : Preface_specs.MONOID) =
Preface_make.Monoid.Via_combine_and_neutral (struct
  type t = T.t

  let neutral = T.neutral
  let combine a b = T.combine b a
end)

module Id = struct
  type 'a t = 'a

  module Functor = Preface_make.Functor.Via_map (struct
    type nonrec 'a t = 'a t

    let map f x = f x
  end)

  module Applicative = Preface_make.Applicative.Via_pure_and_apply (struct
    type nonrec 'a t = 'a t

    let pure x = x
    let apply f x = f x
  end)

  module Monad = Preface_make.Monad.Via_return_and_bind (struct
    type nonrec 'a t = 'a t

    let return x = x
    let bind f x = f x
  end)
end
