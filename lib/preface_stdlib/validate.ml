type 'a t = ('a, exn Nonempty_list.t) Validation.t

let pure = Validation.pure

let valid = Validation.valid

let invalid = Validation.invalid

let error err = invalid (Nonempty_list.create err)

module Functor = Validation.Functor (struct
  type t = exn Nonempty_list.t
end)

module Exn_list = Preface_make.Semigroup.From_alt (Nonempty_list.Alt) (Exn)
module Applicative = Validation.Applicative (Exn_list)
module Selective = Validation.Selective (Exn_list)

module Monad = Validation.Monad (struct
  type t = exn Nonempty_list.t
end)

let case = Validation.case

let to_result = function
  | Validation.Valid x -> Ok x
  | Validation.Invalid x -> Error x
;;

let equal f = Validation.equal f (Nonempty_list.equal Exn.equal)

let pp f = Validation.pp f (Nonempty_list.pp Exn.pp)
