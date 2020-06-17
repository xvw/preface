module Opt = Preface_stdlib.Option.Monad

type 'a t = 'a QCheck.arbitrary

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

let validation ?collect arbitrary =
  let gen = Gen.validation (QCheck.gen arbitrary) in
  let print =
    let open Opt in
    arbitrary.QCheck.print
    >|= fun printer x ->
    let pp_hook ppf x = Format.fprintf ppf "%s" (printer x) in
    Format.asprintf "%a" (Preface_stdlib.Validation.pp pp_hook) x
  in
  QCheck.make ?print ?collect gen
;;

let continuation ?collect l =
  let gen = Gen.continuation (QCheck.gen l) in
  QCheck.make ?collect gen
;;

let stream ?collect l =
  let gen = Gen.stream (QCheck.gen l) in
  QCheck.make ?collect gen
;;

let state l =
  let gen = Gen.state (QCheck.gen l) in
  QCheck.make gen
;;

include QCheck
