open Preface_core.Fun

type ('a, 'b) t = ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

let pure x = Ok x

module Bifunctor = Preface_make.Bifunctor.Via_bimap (struct
  type nonrec ('a, 'b) t = ('a, 'b) t

  let bimap f g = function Ok x -> Ok (f x) | Error x -> Error (g x)
end)

module Functor (T : Preface_specs.Types.T0) =
Preface_make.Functor.Via_map (struct
  type nonrec 'a t = ('a, T.t) t

  let map f x = Bifunctor.bimap f id x
end)

module Applicative (T : Preface_specs.Types.T0) =
Preface_make.Applicative.Via_apply (struct
  module F = Functor (T)

  type nonrec 'a t = ('a, T.t) t

  let pure = pure

  let apply fa xa =
    (match (fa, xa) with (Ok f, x) -> F.map f x | (Error x, _) -> Error x)
  ;;
end)

module Monad (T : Preface_specs.Types.T0) = Preface_make.Monad.Via_bind (struct
  type nonrec 'a t = ('a, T.t) t

  let return = pure

  let bind f = function Ok x -> f x | Error x -> Error x
end)

let equal f g left right =
  match (left, right) with
  | (Ok x, Ok y) -> f x y
  | (Error x, Error y) -> g x y
  | _ -> false
;;

let pp f g formater = function
  | Ok x -> Format.fprintf formater "Ok (%a)" f x
  | Error x -> Format.fprintf formater "Error (%a)" g x
;;
