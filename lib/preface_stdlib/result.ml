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

module Invariant (T : Preface_specs.Types.T0) =
  Preface_make.Invariant.From_functor (Functor (T))

module Alt (T : Preface_specs.Types.T0) =
  Preface_make.Alt.Over_functor
    (Functor
       (T))
       (struct
         type nonrec 'a t = ('a, T.t) t

         let combine x y = match (x, y) with Error _, a -> a | a, _ -> a
       end)

let traverse_aux pure map f = function
  | Error x -> pure (Error x)
  | Ok x -> map (fun x -> Ok x) (f x)
;;

module Applicative (T : Preface_specs.Types.T0) = struct
  module A = Preface_make.Applicative.Via_pure_and_apply (struct
    module F = Functor (T)

    type nonrec 'a t = ('a, T.t) t

    let pure = pure

    let apply fa xa =
      match (fa, xa) with Ok f, x -> F.map f x | Error x, _ -> Error x
    ;;
  end)

  module T (A : Preface_specs.APPLICATIVE) =
    Preface_make.Traversable.Over_applicative
      (A)
      (struct
        type 'a t = 'a A.t
        type 'a iter = ('a, T.t) Bifunctor.t

        let traverse f x = traverse_aux A.pure A.map f x
      end)

  include Preface_make.Traversable.Join_with_applicative (A) (T)
end

module Monad (T : Preface_specs.Types.T0) = struct
  module M = Preface_make.Monad.Via_return_and_bind (struct
    type nonrec 'a t = ('a, T.t) t

    let return = pure
    let bind f = function Ok x -> f x | Error x -> Error x
  end)

  module T (M : Preface_specs.MONAD) =
    Preface_make.Traversable.Over_monad
      (M)
      (struct
        type 'a t = 'a M.t
        type 'a iter = ('a, T.t) Bifunctor.t

        let traverse f x = traverse_aux M.return M.map f x
      end)

  include Preface_make.Traversable.Join_with_monad (M) (T)
end

module Foldable (T : Preface_specs.Types.T0) =
Preface_make.Foldable.Via_fold_right (struct
  type nonrec 'a t = ('a, T.t) t

  let fold_right f x acc = match x with Error _ -> acc | Ok v -> f v acc
end)

let equal f g left right =
  match (left, right) with
  | Ok x, Ok y -> f x y
  | Error x, Error y -> g x y
  | _ -> false
;;

let pp f g formater = function
  | Ok x -> Format.fprintf formater "Ok (%a)" f x
  | Error x -> Format.fprintf formater "Error (%a)" g x
;;
