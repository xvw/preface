module Opt = Preface_stdlib.Option.Monad

type 'a t = 'a QCheck.arbitrary

let exn ?collect () =
  let print = Some (fun x -> Format.asprintf "%a" Preface_stdlib.Exn.pp x) in
  QCheck.make ?print ?collect Gen.exn
;;

let identity ?collect arbitrary =
  let gen = Gen.identity (QCheck.gen arbitrary) in
  let print = Opt.(arbitrary.QCheck.print >|= Print.identity) in
  let shrink = Opt.(arbitrary.QCheck.shrink >|= Shrink.identity) in
  let small =
    let open Opt in
    arbitrary.QCheck.small
    >|= (fun f x -> x |> Preface_stdlib.Identity.extract |> f)
  in
  QCheck.make ?print ?shrink ?small ?collect gen
;;

let either ?collect left right =
  let gen = Gen.either (QCheck.gen left) (QCheck.gen right) in
  let print =
    let open Opt in
    left.QCheck.print
    >>= fun leftp ->
    right.QCheck.print
    >|= fun rightp -> function
    | Preface_stdlib.Either.Left x -> "Left " ^ leftp x
    | Preface_stdlib.Either.Right x -> "Right " ^ rightp x
  in
  QCheck.make ?print ?collect gen
;;

let result ?collect ok error =
  let gen = Gen.result (QCheck.gen ok) (QCheck.gen error) in
  let print =
    let open Opt in
    ok.QCheck.print
    >>= fun okp ->
    error.QCheck.print
    >|= fun errorp -> function
    | Preface_stdlib.Result.Ok x -> "Ok " ^ okp x
    | Preface_stdlib.Result.Error x -> "Error " ^ errorp x
  in
  QCheck.make ?print ?collect gen
;;

let try_ ?collect arbitrary =
  let gen = Gen.try_ (QCheck.gen arbitrary) in
  let print =
    let open Opt in
    arbitrary.QCheck.print
    >|= fun printer x ->
    let pp_hook ppf x = Format.fprintf ppf "%s" (printer x) in
    Format.asprintf "%a" (Preface_stdlib.Try.pp pp_hook) x
  in
  QCheck.make ?print ?collect gen
;;

let nonempty_list ?collect arbitrary =
  let gen = Gen.small_nonempty_list (QCheck.gen arbitrary) in
  let print =
    let open Opt in
    arbitrary.QCheck.print
    >|= fun printer x ->
    let pp_hook ppf x = Format.fprintf ppf "%s" (printer x) in
    Format.asprintf "%a" (Preface_stdlib.Nonempty_list.pp pp_hook) x
  in
  QCheck.make ?print ?collect gen
;;

let validation ?collect valid invalid =
  let gen = Gen.validation (QCheck.gen valid) (QCheck.gen invalid) in
  let print =
    let open Opt in
    valid.QCheck.print
    >>= fun vprinter ->
    invalid.QCheck.print
    >|= fun iprinter -> function
    | Preface_stdlib.Validation.Valid x -> "Valid " ^ vprinter x
    | Preface_stdlib.Validation.Invalid x -> "Invalid " ^ iprinter x
  in
  QCheck.make ?print ?collect gen
;;

let continuation ?collect l =
  let gen = Gen.continuation (QCheck.gen l) in
  QCheck.make ?collect gen
;;

let stream ?collect l =
  let gen = Gen.stream (QCheck.gen l) in
  let print =
    let open Opt in
    l.QCheck.print
    >|= fun printer x ->
    match Preface_stdlib.Stream.take 3 x with
    | Error _ -> "<Stream: Error>"
    | Ok x ->
      let fragment = String.concat "; " (List.map printer x) in
      "<Stream [" ^ fragment ^ " ...]>"
  in
  QCheck.make ?collect ?print gen
;;

let state l =
  let gen = Gen.state (QCheck.gen l) in
  QCheck.make gen
;;

include QCheck
