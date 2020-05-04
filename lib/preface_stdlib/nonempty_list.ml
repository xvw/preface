include Preface_core.Nonempty_list

let pure x = create x

module Functor = Preface_make.Functor.Via_map (struct
  type nonrec 'a t = 'a t

  let map = map
end)

module Applicative = Preface_make.Applicative.Via_apply (struct
  type nonrec 'a t = 'a t

  let pure = pure

  let apply fs xs = flatten @@ map (fun f -> map (fun x -> f x) xs) fs
end)

module Monad = Preface_make.Monad.Via_map_and_join (struct
  type nonrec 'a t = 'a t

  let return = pure

  let map = map

  let join = flatten
end)

module Selective =
  Preface_make.Selective.Over_applicative
    (Applicative)
    (Preface_make.Selective.Select_from_monad (Monad))
