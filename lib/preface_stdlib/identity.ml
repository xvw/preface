type 'a t = 'a

let pure x = x

module Functor = Preface_make.Functor.Via_map (struct
  type nonrec 'a t = 'a t

  let map f = f
end)

module Applicative = Preface_make.Applicative.Via_apply (struct
  type nonrec 'a t = 'a t

  let pure = pure

  let apply f = f
end)

module Monad = Preface_make.Monad.Via_bind (struct
  type nonrec 'a t = 'a t

  let return = pure

  let bind f = f
end)

let eq f a b = f a b

let pp pp' formater a = Format.fprintf formater "Identity (%a)" pp' a
