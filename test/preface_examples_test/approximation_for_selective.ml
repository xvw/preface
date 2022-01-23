module Monoid = Preface.Make.Monoid.Via_combine_and_neutral (struct
  type t = string

  let neutral = ""
  let combine x y = x ^ y
end)

module Over = Preface.Approximation.Over (Monoid)
module Under = Preface.Approximation.Under (Monoid)

let over_approximation_with_if () =
  let open Over in
  let (Over computed) =
    let open Selective in
    if_ (Over "a") (Over "b") (Over "c")
    *> Over "d"
    *> when_ (Over "e") (Over "f")
  in
  let expected = "abcdef" in
  Alcotest.(check string) "should be equal" expected computed
;;

let under_approximation_with_if () =
  let open Under in
  let (Under computed) =
    let open Selective in
    if_ (Under "a") (Under "b") (Under "c")
    *> Under "d"
    *> when_ (Under "e") (Under "f")
  in
  let expected = "ade" in
  Alcotest.(check string) "should be equal" expected computed
;;

let cases =
  let open Alcotest in
  [
    ( "Over Approximation"
    , [
        test_case "over approximation with Selective.if_" `Quick
          over_approximation_with_if
      ] )
  ; ( "Under Approximation"
    , [
        test_case "under approximation with Selective.if_" `Quick
          under_approximation_with_if
      ] )
  ]
;;
