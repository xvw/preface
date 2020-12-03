type 'a t = ('a, exn) Result.t

let pure x = Ok x

let ok = pure

let error exn = Error exn

module Functor = Result.Functor (struct
  type t = exn
end)

module Applicative = Result.Applicative (struct
  type t = exn
end)

module Monad = Result.Monad (struct
  type t = exn
end)

let capture f = (try ok (f ()) with exn -> error exn)

let case f g = function Ok x -> f x | Error exn -> g exn

let to_validation = function
  | Ok x -> Validation.valid x
  | Error exn -> Validation.invalid (Nonempty_list.create exn)
;;

let equal f = Result.equal f Exn.equal

let pp pp' = Result.pp pp' Exn.pp
