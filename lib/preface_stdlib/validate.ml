type 'a t = ('a, exn Nonempty_list.t) Validation.t

let pure = Validation.pure

let valid = Validation.valid

let invalid = Validation.invalid

module Functor = Validation.Functor (struct
  type t = exn Nonempty_list.t
end)

module Applicative =
  Validation.Applicative
    (Nonempty_list.Alt)
    (struct
      type t = exn
    end)

module Selective =
  Validation.Selective
    (Nonempty_list.Alt)
    (struct
      type t = exn
    end)

module Monad = Validation.Monad (struct
  type t = exn Nonempty_list.t
end)

let case = Validation.case

let to_result = function
  | Validation.Valid x -> Ok x
  | Validation.Invalid x -> Error x
;;

let eq f = Validation.eq f (Nonempty_list.eq Exn.eq)

let pp f = Validation.pp f (Nonempty_list.pp Exn.pp)
