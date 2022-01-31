type 'a t = 'a

let pure x = x
let extract x = x

module Functor = Preface_make.Functor.Via_map (struct
  type nonrec 'a t = 'a t

  let map f = f
end)

module Applicative = Preface_make.Applicative.Via_pure_and_apply (struct
  type nonrec 'a t = 'a t

  let pure = pure
  let apply f = f
end)

module Monad = Preface_make.Monad.Via_return_and_bind (struct
  type nonrec 'a t = 'a t

  let return = pure
  let bind f = f
end)

module Selective =
  Preface_make.Selective.Over_applicative_via_select
    (Applicative)
    (Preface_make.Selective.Select_from_monad (Monad))

module Invariant = Preface_make.Invariant.From_functor (Functor)

module Comonad = Preface_make.Comonad.Via_map_and_duplicate (struct
  type nonrec 'a t = 'a t

  let extract = extract
  let map f = f
  let duplicate x = x
end)

let equal f a b = f a b
let pp pp' formater a = Format.fprintf formater "Identity (%a)" pp' a
