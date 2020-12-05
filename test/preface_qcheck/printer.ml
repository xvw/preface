type 'a t = 'a QCheck.Print.t

let to_string pp ppf x = Format.fprintf ppf "%s" (pp x)

let identity pp x =
  Format.asprintf "%a" (Preface_stdlib.Identity.pp (to_string pp)) x
;;

let either pp_left pp_right x =
  Format.asprintf "%a"
    (Preface_stdlib.Either.pp (to_string pp_left) (to_string pp_right))
    x
;;

let result pp_left pp_right x =
  Format.asprintf "%a"
    (Preface_stdlib.Result.pp (to_string pp_left) (to_string pp_right))
    x
;;

let validation pp_left pp_right x =
  Format.asprintf "%a"
    (Preface_stdlib.Validation.pp (to_string pp_left) (to_string pp_right))
    x
;;

let exn e = Format.asprintf "%a " Preface_stdlib.Exn.pp e

let try_ x = result x exn

let nonempty_list pp x =
  Format.asprintf "%a" (Preface_stdlib.Nonempty_list.pp (to_string pp)) x
;;

let validate x = validation x (nonempty_list exn)

let stream pp x =
  let fragment = Preface_stdlib.Stream.take 5 x in
  match fragment with
  | Error _ -> Format.asprintf "%s" (try_ (QCheck.Print.list pp) fragment)
  | Ok l ->
    let sublist = List.map pp l in
    "Stream(" ^ String.concat "; " sublist ^ "; ...)"
;;

include (QCheck.Print : module type of QCheck.Print with type 'a t := 'a t)
