type 'a t = ('a, exn) result

let pure x = Ok x

let ok = pure

let error exn = Error exn

module Functor = Preface_make.Functor.Via_map (struct
  type nonrec 'a t = 'a t

  let map f = function Ok x -> Ok (f x) | Error x -> Error x
end)

module Applicative = Preface_make.Applicative.Via_apply (struct
  type nonrec 'a t = 'a t

  let pure = pure

  let apply fs xs =
    match (fs, xs) with
    | (Ok f, x) -> Functor.map f x
    | (Error l, _) -> Error l
  ;;
end)

module Monad = Preface_make.Monad.Via_bind (struct
  type nonrec 'a t = 'a t

  let return = pure

  let bind f = function Ok x -> f x | Error x -> Error x
end)

let capture f = (try ok (f ()) with exn -> error exn)

let case f g = function Ok x -> f x | Error exn -> g exn

let to_validation = function Ok x -> Ok x | Error exn -> Error [ exn ]

let eq f a b =
  match (a, b) with
  | (Ok x, Ok y) -> f x y
  | (Error x, Error y) -> Printexc.(exn_slot_id x = exn_slot_id y)
  | _ -> false
;;

let pp pp' formater = function
  | Error exn -> Format.fprintf formater "Error (%s)" (Printexc.to_string exn)
  | Ok x -> Format.fprintf formater "Ok (%a)" pp' x
;;
