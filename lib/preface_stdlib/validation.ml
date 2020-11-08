type ('a, 'errors) t =
  | Valid of 'a
  | Invalid of 'errors

let valid x = Valid x

let invalid errors = Invalid errors

let pure = valid

let case f g = function Valid x -> f x | Invalid x -> g x

module Bifunctor = Preface_make.Bifunctor.Via_bimap (struct
  type nonrec ('a, 'errors) t = ('a, 'errors) t

  let bimap f g = function Valid x -> Valid (f x) | Invalid x -> Invalid (g x)
end)

module Functor (T : Preface_specs.Types.T0) =
Preface_make.Functor.Via_map (struct
  type nonrec 'a t = ('a, T.t) t

  let map f = function Valid x -> Valid (f x) | Invalid err -> Invalid err
end)

module Applicative (Alt : Preface_specs.ALT) (Error : Preface_specs.Types.T0) =
Preface_make.Applicative.Via_apply (struct
  type nonrec 'a t = ('a, Error.t Alt.t) t

  let pure = valid

  let apply fx xs =
    match (fx, xs) with
    | (Valid f, Valid x) -> Valid (f x)
    | (Invalid left, Invalid right) -> Invalid (Alt.combine left right)
    | (Invalid x, _) | (_, Invalid x) -> Invalid x
  ;;
end)

module Selective (Alt : Preface_specs.ALT) (Error : Preface_specs.Types.T0) =
struct
  module A = Applicative (Alt) (Error)

  module S =
    Preface_make.Selective.Over_applicative
      (A)
      (struct
        type nonrec 'a t = ('a, Error.t Alt.t) t

        let pure = valid

        type ('a, 'b) either = ('a, 'b) Preface_core.Either.t =
          | Left of 'a
          | Right of 'b

        let select either f =
          match either with
          | Valid (Left a) -> A.map (( |> ) a) f
          | Valid (Right b) -> Valid b
          | Invalid err -> Invalid err
        ;;
      end)

  include S
end

module Monad (T : Preface_specs.Types.T0) = Preface_make.Monad.Via_bind (struct
  type nonrec 'a t = ('a, T.t) t

  let return = valid

  let bind f = function Valid x -> f x | Invalid err -> Invalid err
end)

let eq f g left right =
  match (left, right) with
  | (Valid x, Valid y) -> f x y
  | (Invalid x, Invalid y) -> g x y
  | _ -> false
;;

let pp f g formater = function
  | Valid x -> Format.fprintf formater "Valid (%a)" f x
  | Invalid x -> Format.fprintf formater "Invalid (%a)" g x
;;
