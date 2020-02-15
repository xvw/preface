type 'a t = ('a, exn list) result

let pure x = Ok x

let ok = pure

let error exns = Error exns

module Functor = Preface_make.Functor.Via_map (struct
  type nonrec 'a t = 'a t

  let map f = function Ok x -> Ok (f x) | Error x -> Error x
end)

module Applicative = Preface_make.Applicative.Via_apply (struct
  type nonrec 'a t = 'a t

  let pure = pure

  let apply fx xs =
    match (fx, xs) with
    | (Ok f, Ok x) -> Ok (f x)
    | (Error left, Error right) -> Error (left @ right)
    | (Error x, _) | (_, Error x) -> Error x
  ;;
end)

module Monad = Preface_make.Monad.Via_bind (struct
  type nonrec 'a t = 'a t

  let return = pure

  let bind f = function Ok x -> f x | Error x -> Error x
end)

let capture f = (try ok (f ()) with exn -> error [ exn ])

let case f g = function Ok x -> f x | Error exns -> g exns

let eq f a b =
  match (a, b) with
  | (Ok x, Ok y) -> f x y
  | (Error xs, Error ys) ->
    List.eq (fun x y -> Printexc.(exn_slot_id x = exn_slot_id y)) xs ys
  | _ -> false
;;

let pp pp' formater = function
  | Error exn ->
    let exn_str = List.Functor.map Printexc.to_string exn in
    Format.(fprintf formater "Error %a" (List.pp pp_print_string) exn_str)
  | Ok x -> Format.fprintf formater "Ok (%a)" pp' x
;;
