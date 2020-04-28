type 'a t = 'a option

let pure x = Some x

module Functor = Preface_make.Functor.Via_map (struct
  type nonrec 'a t = 'a t

  let map f = function Some x -> Some (f x) | None -> None
end)

module Applicative = Preface_make.Applicative.Via_apply (struct
  type nonrec 'a t = 'a t

  let pure = pure

  let apply fa xa =
    (match (fa, xa) with (Some f, Some x) -> Some (f x) | _ -> None)
  ;;
end)

module Monad = Preface_make.Monad.Via_bind (struct
  type nonrec 'a t = 'a t

  let return x = Some x

  let bind f = function Some x -> f x | None -> None
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
