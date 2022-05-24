open Preface_core.Fun
include Preface_core.Shims.Either

let pure x = Right x

module Bifunctor = Preface_make.Bifunctor.Via_bimap (struct
  type nonrec ('a, 'b) t = ('a, 'b) t

  let bimap f g = function Left x -> Left (f x) | Right x -> Right (g x)
end)

module Functor (T : Preface_specs.Types.T0) =
Preface_make.Functor.Via_map (struct
  type nonrec 'a t = (T.t, 'a) t

  let map f x = Bifunctor.bimap id f x
end)

let traverse_aux pure map f = function
  | Left x -> pure (Left x)
  | Right x -> map right (f x)
;;

module Alt (T : Preface_specs.Types.T0) =
  Preface_make.Alt.Over_functor
    (Functor
       (T))
       (struct
         type nonrec 'a t = (T.t, 'a) t

         let combine x y = match (x, y) with Left _, a -> a | a, _ -> a
       end)

module Applicative (T : Preface_specs.Types.T0) = struct
  module A = Preface_make.Applicative.Via_pure_and_apply (struct
    module F = Functor (T)

    type nonrec 'a t = (T.t, 'a) t

    let pure = pure

    let apply fa xa =
      match (fa, xa) with Right f, x -> F.map f x | Left x, _ -> Left x
    ;;
  end)

  module T (A : Preface_specs.APPLICATIVE) =
    Preface_make.Traversable.Over_applicative
      (A)
      (struct
        type 'a t = 'a A.t
        type 'a iter = (T.t, 'a) Bifunctor.t

        let traverse f x = traverse_aux A.pure A.map f x
      end)

  include Preface_make.Traversable.Join_with_applicative (A) (T)
end

module Monad (T : Preface_specs.Types.T0) = struct
  module M = Preface_make.Monad.Via_return_and_bind (struct
    type nonrec 'a t = (T.t, 'a) t

    let return = pure
    let bind f = function Right x -> f x | Left x -> Left x
  end)

  module T (M : Preface_specs.MONAD) =
    Preface_make.Traversable.Over_monad
      (M)
      (struct
        type 'a t = 'a M.t
        type 'a iter = (T.t, 'a) Bifunctor.t

        let traverse f x = traverse_aux M.return M.map f x
      end)

  include Preface_make.Traversable.Join_with_monad (M) (T)
end

module Selective (T : Preface_specs.Types.T0) =
  Preface_make.Selective.Over_applicative_via_select
    (Applicative(T))
    (Preface_make.Selective.Select_from_monad (Monad(T)))

module Invariant (T : Preface_specs.Types.T0) =
  Preface_make.Invariant.From_functor (Functor (T))

module Foldable (T : Preface_specs.Types.T0) =
Preface_make.Foldable.Via_fold_right (struct
  type nonrec 'a t = (T.t, 'a) t

  let fold_right f x acc = match x with Left _ -> acc | Right v -> f v acc
end)

let equal f g left right =
  match (left, right) with
  | Left x, Left y -> f x y
  | Right x, Right y -> g x y
  | _ -> false
;;

let pp f g formater = function
  | Left x -> Format.fprintf formater "Left (%a)" f x
  | Right x -> Format.fprintf formater "Right (%a)" g x
;;
