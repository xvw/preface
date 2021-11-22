type 'a t = 'a option

let pure x = Some x

module Foldable = Preface_make.Foldable.Via_fold_right (struct
  type nonrec 'a t = 'a t

  let fold_right f x acc = (match x with None -> acc | Some v -> f v acc)
end)

module Functor = Preface_make.Functor.Via_map (struct
  type nonrec 'a t = 'a t

  let map f = function Some x -> Some (f x) | None -> None
end)

module Alternative = Preface_make.Alternative.Via_apply (struct
  type nonrec 'a t = 'a t

  let pure = pure

  let apply fa xa =
    (match (fa, xa) with (Some f, Some x) -> Some (f x) | _ -> None)
  ;;

  let neutral = None

  let combine l r = (match (l, r) with (None, x) -> x | (x, _) -> x)
end)

let traverse_aux pure map f = function
  | None -> pure None
  | Some x -> map Stdlib.Option.some (f x)
;;

module Applicative_traversable (A : Preface_specs.APPLICATIVE) =
  Preface_make.Traversable.Over_applicative
    (A)
    (struct
      type 'a t = 'a A.t

      type 'a iter = 'a option

      let traverse f x = traverse_aux A.pure A.map f x
    end)

module Applicative =
  Preface_make.Traversable.Join_with_applicative
    (Alternative)
    (Applicative_traversable)

module Monad_internal = Preface_make.Monad.Via_bind (struct
  type nonrec 'a t = 'a t

  let return x = Some x

  let bind f = function Some x -> f x | None -> None
end)

module Monad_traversable (M : Preface_specs.MONAD) =
  Preface_make.Traversable.Over_monad
    (M)
    (struct
      type 'a t = 'a M.t

      type 'a iter = 'a option

      let traverse f x = traverse_aux M.return M.map f x
    end)

module Monad =
  Preface_make.Traversable.Join_with_monad (Monad_internal) (Monad_traversable)
module Monad_plus =
  Preface_make.Monad_plus.Over_monad_and_alternative (Monad) (Alternative)
module Invariant = Preface_make.Invariant.From_functor (Functor)

module Monoid (M : Preface_specs.SEMIGROUP) =
Preface_make.Monoid.Via_combine_and_neutral (struct
  type nonrec t = M.t t

  let neutral = None

  let combine x y =
    match (x, y) with
    | (None, result) | (result, None) -> result
    | (Some a, Some b) -> Some (M.combine a b)
  ;;
end)

let equal f left right =
  match (left, right) with
  | (None, None) -> true
  | (Some x, Some y) -> f x y
  | _ -> false
;;

let pp pp' formater = function
  | None -> Format.fprintf formater "None"
  | Some x -> Format.fprintf formater "Some (%a)" pp' x
;;

let if_ p value = if p value then Some value else None

let unless p = if_ (Predicate.negate p)

let or_ a b = (match a with Some x -> Some x | None -> b)
