type 'a t = 'a

let pure x = x

module Functor = Functor.Make (struct
  type nonrec 'a t = 'a t

  let map f = f
end)

(* Trick to avoid Diamond problem:
   http://gallium.inria.fr/blog/overriding-submodules/
*)
include (Functor : module type of Functor with type 'a t := 'a t)

let eq f a b = f a b

let pp pp' formater a = Format.fprintf formater "Identity (%a)" pp' a
