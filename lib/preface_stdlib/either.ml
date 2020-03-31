type ('a, 'b) t =
  | Left of 'a
  | Right of 'b

let pure x = Right x

module EitherF (F : Preface_specs.FUNCTOR) (G : Preface_specs.FUNCTOR) =
Preface_make.Functor.Via_map (struct
  type nonrec 'a t = ('a F.t, 'a G.t) t

  let map f = function
    | Left x -> Left (F.map f x)
    | Right x -> Right (G.map f x)
  ;;
end)

module Bifunctor = Preface_make.Bifunctor.Via_bimap (struct
  type nonrec ('a, 'b) t = ('a, 'b) t

  let bimap f g = function Left x -> Left (f x) | Right x -> Right (g x)
end)

module Functor (T : Preface_specs.Requirements.Type) :
  Preface_specs.FUNCTOR with type 'a t = (T.t, 'a) t =
  EitherF (Preface_make.Functor.Const (T)) (Preface_make.Functor.Id)

module Applicative (T : sig
  type t
end) =
Preface_make.Applicative.Via_apply (struct
  module F = Functor (T)

  type nonrec 'a t = (T.t, 'a) t

  let pure = pure

  let apply fa xa =
    (match (fa, xa) with (Right f, x) -> F.map f x | (Left x, _) -> Left x)
  ;;
end)

module Monad (T : sig
  type t
end) =
Preface_make.Monad.Via_bind (struct
  type nonrec 'a t = (T.t, 'a) t

  let return = pure

  let bind f = function Right x -> f x | Left x -> Left x
end)

let left x = Left x

let right = pure

let map_left = Bifunctor.fst

let map_right = Bifunctor.snd

let map = Bifunctor.snd

let map_both = Bifunctor.bimap
