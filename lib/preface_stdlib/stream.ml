type 'a t = C of 'a * 'a t Lazy.t

let stream a l = C (a, l)

let rec pure x = C (x, lazy (pure x))

let repeat = pure

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

let at i s =
  let open Try.Monad.Syntax in
  let* index = Exn.check_position i in
  let rec aux_at i s =
    if i = 0 then Try.ok (hd s) else aux_at (pred i) (tl s)
  in
  aux_at index s
;;

let drop i s =
  let open Try.Monad.Syntax in
  let* index = Exn.check_position i in
  let rec aux_drop i s = if i = 0 then Try.ok s else aux_drop (pred i) (tl s) in
  aux_drop index s
;;

let take i s =
  let open Try.Monad.Syntax in
  let* index = Exn.check_position i in
  let rec aux_take acc i s =
    if i = 0
    then Stdlib.List.rev acc
    else aux_take (hd s :: acc) (pred i) (tl s)
  in
  Try.ok (aux_take [] index s)
;;

let take_while predicate stream =
  let rec take_aux acc = function
    | C (x, xs) ->
      if predicate x
      then take_aux (x :: acc) (Lazy.force xs)
      else Stdlib.List.rev acc
  in
  take_aux [] stream
;;

let drop_while predicate stream =
  let rec drop_aux = function
    | C (x, xs) as stream ->
      if predicate x then drop_aux (Lazy.force xs) else stream
  in
  drop_aux stream
;;

module Infix = struct
  let ( <:> ) = cons

  let ( .%[] ) s i = at i s
end

include Infix
