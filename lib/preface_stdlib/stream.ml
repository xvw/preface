type 'a t = C of 'a * 'a t Lazy.t

exception Negative_position of int

let stream a l = C (a, l)

let rec pure x = C (x, lazy (pure x))

let cons x s = C (x, lazy s)

let hd = function C (x, _) -> x

let tl = function C (_, xs) -> Lazy.force xs

let rec map f = function C (x, xs) -> C (f x, lazy (map f @@ Lazy.force xs))

module Functor = Preface_make.Functor.Via_map (struct
  type nonrec 'a t = 'a t

  let map = map
end)

module Applicative = Preface_make.Applicative.Via_apply (struct
  type nonrec 'a t = 'a t

  let pure = pure

  let rec apply stream1 stream2 =
    let f g x = g x in
    match (stream1, stream2) with
    | (C (x, xs), C (y, ys)) ->
      C (f x y, lazy (apply (Lazy.force xs) (Lazy.force ys)))
  ;;
end)

module Monad = Preface_make.Monad.Via_map_and_join (struct
  type nonrec 'a t = 'a t

  let return = pure

  let map = map

  let rec join = function
    | C (first, second) ->
      let tail = lazy (map tl @@ Lazy.force second) in
      C (hd first, lazy (join @@ Lazy.force tail))
  ;;
end)

module Comonad = Preface_make.Comonad.Via_map_and_duplicate (struct
  type nonrec 'a t = 'a t

  let extract = hd

  let rec duplicate = function
    | C (a, s) -> C (C (a, s), lazy (duplicate @@ Lazy.force s))
  ;;

  let rec map f = function C (a, s) -> C (f a, lazy (map f @@ Lazy.force s))
end)

let rec at i s =
  if i < 0
  then Try.error (Negative_position i)
  else if i = 0
  then Try.ok (hd s)
  else at (pred i) (tl s)
;;

let rec drop i s =
  if i < 0
  then Try.error (Negative_position i)
  else if i = 0
  then Try.ok s
  else drop (pred i) (tl s)
;;

module Infix = struct
  let ( <:> ) = cons

  let ( .%[] ) s i = at i s
end

include Infix
