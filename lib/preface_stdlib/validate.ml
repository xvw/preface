type 'a t = ('a, exn Nonempty_list.t) Validation.t

let pure = Validation.pure

let valid = Validation.valid

let invalid = Validation.invalid

let error err = invalid (Nonempty_list.create err)

module Exn_list = Preface_make.Semigroup.From_alt (Nonempty_list.Alt) (Exn)
module Functor = Validation.Functor (Exn_list)
module Invariant = Preface_make.Invariant.From_functor (Functor)
module Alt = Validation.Alt (Exn_list)
module Applicative = Validation.Applicative (Exn_list)
module Selective = Validation.Selective (Exn_list)
module Monad = Validation.Monad (Exn_list)
module Foldable = Validation.Foldable (Exn_list)

let case = Validation.case

let to_result = function
  | Validation.Valid x -> Ok x
  | Validation.Invalid x -> Error x
;;

let equal f = Validation.equal f (Nonempty_list.equal Exn.equal)

let pp f = Validation.pp f (Nonempty_list.pp Exn.pp)
