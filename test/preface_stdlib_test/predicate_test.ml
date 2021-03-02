open Preface

let even x = x mod 2 = 0

let test_negate () =
  let expected = true
  and computed = (Predicate.negate even) 15 in
  Alcotest.(check bool) "should be equal" computed expected
;;

let test_tautology () =
  let expected = true
  and computed = Predicate.tautology 14 in
  Alcotest.(check bool) "should be equal" computed expected
;;

let test_contradiction () =
  let expected = false
  and computed = Predicate.contradiction "str" in
  Alcotest.(check bool) "should be equal" computed expected
;;

let cases =
  [
    ( "Predicate"
    , let open Alcotest in
      [
        test_case "negate" `Quick test_negate
      ; test_case "tautology" `Quick test_tautology
      ; test_case "contradiction" `Quick test_contradiction
      ] )
  ]
;;
