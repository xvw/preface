module type GENERATOR = sig
  type input

  val arbitrary : input QCheck.arbitrary

  val observable : input QCheck.Observable.t
end

module type REQ = sig
  type 'a t

  val suite_name : string

  val arbitrary : 'a QCheck.arbitrary -> 'a t QCheck.arbitrary
end

module type HOOK = sig
  type 'a t

  val apply : 'a t -> 'c
end

module type ALCOTEST_SUITE = sig
  val cases : string * unit Alcotest.test_case list
end

let rec fold f acc i = if i = 0 then acc else fold f (f acc i) (i - 1)

module Nel = Preface_core.Nonempty_list

module Gen = struct
  type 'a t = 'a QCheck.Gen.t

  let option ?(distribution = 0.15) f state =
    let prob = Random.State.float state 1.0 in
    if prob < distribution then None else Some (f state)
  ;;

  let either ?(distribution = 0.5) f_left f_right state =
    let prob = Random.State.float state 1.0 in
    let open Preface_stdlib.Either in
    if prob < distribution then Left (f_left state) else Right (f_right state)
  ;;

  let nonempty_list_size size gen st =
    let init = Nel.Last (gen st) in
    fold (fun acc _ -> Nel.(gen st :: acc)) init (size st)
  ;;

  let nonempty_list g s = nonempty_list_size QCheck.Gen.nat g s

  let small_nonempty_list g s = nonempty_list_size QCheck.Gen.small_nat g s

  let continuation f state = Preface_stdlib.Continuation.pure (f state)

  let stream f state = Preface_stdlib.Stream.pure (f state)

  exception A

  exception B

  exception C of int

  exception D of float

  exception E of string

  exception F of int list

  exception G of float list

  exception H of string list

  let small_string = QCheck.Gen.(string_size ~gen:printable small_nat)

  let exn state =
    let prob = Random.State.float state 8.0 in
    if prob >= 7.0
    then A
    else if prob >= 6.0
    then B
    else if prob >= 5.0
    then C QCheck.Gen.(generate1 small_int)
    else if prob >= 4.0
    then D QCheck.Gen.(generate1 float)
    else if prob >= 3.0
    then E (QCheck.Gen.generate1 small_string)
    else if prob >= 2.0
    then F QCheck.Gen.(generate1 (small_list small_int))
    else if prob >= 1.0
    then G QCheck.Gen.(generate1 (small_list float))
    else H (QCheck.Gen.generate1 (QCheck.Gen.small_list small_string))
  ;;

  let validation ?(distribution = 0.15) f_ok state =
    let prob = Random.State.float state 1.0 in
    if prob < distribution
    then Error (QCheck.Gen.list exn state)
    else Ok (f_ok state)
  ;;

  let try_ ?(distribution = 0.15) f_ok state =
    let prob = Random.State.float state 1.0 in
    if prob < distribution then Error (exn state) else Ok (f_ok state)
  ;;

  let identity g = Obj.magic g

  let state f state s = (f state, s)
end

module Arbitrary = struct
  type 'a t = 'a QCheck.arbitrary

  let identity a = Obj.magic a

  let either l r =
    let gen = Gen.either (QCheck.gen l) (QCheck.gen r) in
    QCheck.make gen
  ;;

  let try_ l =
    let gen = Gen.try_ (QCheck.gen l) in
    let print =
      l.QCheck.print
      |> Option.map (fun ppl x ->
             let open Preface_stdlib.Try in
             let sub = Functor.(ppl <$> x) in
             Format.asprintf "%a" (pp Format.pp_print_string) sub)
    in
    QCheck.make ?print gen
  ;;

  let validation l =
    let gen = Gen.validation (QCheck.gen l) in
    let print =
      l.QCheck.print
      |> Option.map (fun ppl x ->
             let open Preface_stdlib.Validation in
             let sub = Functor.(ppl <$> x) in
             Format.asprintf "%a" (pp Format.pp_print_string) sub)
    in
    QCheck.make ?print gen
  ;;

  let continuation l =
    let gen = Gen.continuation (QCheck.gen l) in
    QCheck.make gen
  ;;

  let nonempty_list l =
    let gen = Gen.small_nonempty_list (QCheck.gen l) in
    QCheck.make gen
  ;;

  let stream l =
    let gen = Gen.stream (QCheck.gen l) in
    QCheck.make gen
  ;;

  let state l =
    let gen = Gen.state (QCheck.gen l) in
    QCheck.make gen
  ;;
end

module Sample = struct
  module Int = struct
    type input = int

    let arbitrary = QCheck.int

    let observable = QCheck.Observable.int
  end

  module String = struct
    type input = string

    let arbitrary = QCheck.string

    let observable = QCheck.Observable.string
  end
end
