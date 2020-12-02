type 'a t = ('a, exn) Result.t

let ok x = Ok x

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

let eq f = Result.eq f Exn.eq

let pp pp' = Result.pp pp' Exn.pp

include (
  Preface_make.Package.From_applicative_and_monad (Applicative) (Monad) :
      Preface_specs.Package.APPLICATIVE_AND_MONAD with type 'a t := 'a t )
