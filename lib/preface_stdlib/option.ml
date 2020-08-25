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

module Applicative = Preface_make.Applicative.From_alternative (Alternative)

module Monad = Preface_make.Monad.Via_bind (struct
  type nonrec 'a t = 'a t

  let return x = Some x

  let bind f = function Some x -> f x | None -> None
end)

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

let eq f left right =
  match (left, right) with
  | (None, None) -> true
  | (Some x, Some y) -> f x y
  | _ -> false
;;

let pp pp' formater = function
  | None -> Format.fprintf formater "None"
  | Some x -> Format.fprintf formater "Some (%a)" pp' x
;;
