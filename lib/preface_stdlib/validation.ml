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

module Selective =
  Preface_make.Selective.Over_applicative
    (Applicative)
    (struct
      type nonrec 'a t = 'a t

      type ('a, 'b) either = ('a, 'b) Preface_core.Either.t =
        | Left of 'a
        | Right of 'b

      let pure = pure

      let select either f =
        match either with
        | Ok (Left a) -> Applicative.(( |> ) a <$> f)
        | Ok (Right b) -> Ok b
        | Error e -> Error e
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
  | (Error xs, Error ys) -> List.eq Exn.eq xs ys
  | _ -> false
;;

let pp pp' formater = function
  | Error exn -> Format.(fprintf formater "Error %a" (List.pp Exn.pp) exn)
  | Ok x -> Format.fprintf formater "Ok (%a)" pp' x
;;
