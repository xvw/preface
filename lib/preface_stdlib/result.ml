open Preface_core.Fun

type ('a, 'b) t = ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

let pure x = Ok x

module Bifunctor = Preface_make.Bifunctor.Via_bimap (struct
  type nonrec ('a, 'b) t = ('a, 'b) t

  let bimap f g = function Ok x -> Ok (f x) | Error x -> Error (g x)
end)

module Functor = Preface_make.Indexed_functor.Via_map (struct
  type nonrec ('a, 'b) t = ('a, 'b) t

  let map f x = Bifunctor.bimap f id x
end)

module Alt =
  Preface_make.Indexed_alt.Over_functor
    (Functor)
    (struct
      type nonrec ('a, 'b) t = ('a, 'b) t

      let combine x y = match (x, y) with Error _, a -> a | a, _ -> a
    end)

module Applicative = Preface_make.Indexed_applicative.Via_pure_and_apply (struct
  type nonrec ('a, 'b) t = ('a, 'b) t

  let pure = pure

  let apply fa xa =
    match (fa, xa) with Ok f, x -> Functor.map f x | Error x, _ -> Error x
  ;;
end)

module Monad = Preface_make.Indexed_monad.Via_return_and_bind (struct
  type nonrec ('a, 'b) t = ('a, 'b) t

  let return = pure
  let bind f = function Ok x -> f x | Error x -> Error x
end)

module Selective =
  Preface_make.Indexed_selective.Over_applicative_via_select
    (Applicative)
    (Preface_make.Indexed_selective.Select_from_monad (Monad))

module Foldable = Preface_make.Indexed_foldable.Via_fold_right (struct
  type nonrec ('a, 'b) t = ('a, 'b) t

  let fold_right f x acc = match x with Error _ -> acc | Ok v -> f v acc
end)

module Mono (T : Preface_specs.Types.T0) = struct
  module Functor = Preface_make.Functor.Via_map (struct
    type nonrec 'a t = ('a, T.t) t

    let map f x = Bifunctor.bimap f id x
  end)

  module Invariant = Preface_make.Invariant.From_functor (Functor)

  module Alt =
    Preface_make.Alt.Over_functor
      (Functor)
      (struct
        type nonrec 'a t = ('a, T.t) t

        let combine x y = match (x, y) with Error _, a -> a | a, _ -> a
      end)

  let traverse_aux pure map f = function
    | Error x -> pure (Error x)
    | Ok x -> map (fun x -> Ok x) (f x)
  ;;

  module Applicative = struct
    module A = Preface_make.Applicative.Via_pure_and_apply (struct
      module F = Functor

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

  module Monad = struct
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

  module Selective =
    Preface_make.Selective.Over_applicative_via_select
      (Applicative)
      (Preface_make.Selective.Select_from_monad (Monad))

  module Foldable = Preface_make.Foldable.Via_fold_right (struct
    type nonrec 'a t = ('a, T.t) t

    let fold_right f x acc = match x with Error _ -> acc | Ok v -> f v acc
  end)
end

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
