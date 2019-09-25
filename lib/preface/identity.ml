type 'a t = 'a

let pure x = x

module Functor = Functor.Make (struct
  type nonrec 'a t = 'a t

  let map f = f
end)

module Applicative = Applicative.Make_via_apply (struct
  type nonrec 'a t = 'a t

  let pure = pure

  let apply f = f
end)

let eq f a b = f a b

let pp pp' formater a = Format.fprintf formater "Identity (%a)" pp' a
