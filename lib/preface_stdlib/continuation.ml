open Preface_core.Fun.Infix

type 'a t = { run : 'r. ('a -> 'r) -> 'r }

let pure c = { run = (fun k -> k c) }

let map f c = { run = (fun k -> c.run @@ (f %> k)) }

module Functor = Preface_make.Functor.Via_map (struct
  type nonrec 'a t = 'a t

  let map = map
end)

module Applicative = Preface_make.Applicative.Via_map_and_product (struct
  type nonrec 'a t = 'a t

  let pure = pure

  let map = map

  let product ca cb =
    { run = (fun k -> ca.run (fun a -> cb.run (fun b -> k (a, b)))) }
  ;;
end)

module Monad = Preface_make.Monad.Via_map_and_join (struct
  type nonrec 'a t = 'a t

  let return = pure

  let map = map

  let join c = { run = (fun k -> c.run (fun c' -> c'.run k)) }
end)
