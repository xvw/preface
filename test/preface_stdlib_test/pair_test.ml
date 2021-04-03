open Preface

let test_fst () =
  let expected = 10
  and computed = Pair.fst (10, 11) in
  Alcotest.(check int) "should be equal" computed expected
;;

let test_snd () =
  let expected = 10
  and computed = Pair.snd (11, 10) in
  Alcotest.(check int) "should be equal" computed expected
;;

let test_swap () =
  let expected = ("aaa", 10)
  and computed = Pair.swap (10, "aaa") in
  Alcotest.(check (pair string int)) "should be equal" computed expected
;;

let test_curry () =
  let expected = (10, "aaa")
  and computed = Pair.curry (fun (x, y) -> (y, x)) "aaa" 10 in
  Alcotest.(check (pair int string)) "should be equal" computed expected
;;

let test_uncurry () =
  let expected = 30
  and computed = Pair.uncurry ( + ) (10, 20) in
  Alcotest.(check int) "should be equal" computed expected
;;

let test_and () =
  let expected = (1, "foo")
  and computed = Pair.(1 & "foo") in
  Alcotest.(check (pair int string)) "should be equal" computed expected
;;

let test_and_sequence () =
  let computed = Stdlib.List.map Pair.(( & ) "foo") [ 1; 2; 3; 4 ]
  and expected = [ ("foo", 1); ("foo", 2); ("foo", 3); ("foo", 4) ] in
  Alcotest.(check (list (pair string int))) "should be equal" computed expected
;;

let cases =
  [
    ( "Pair"
    , let open Alcotest in
      [
        test_case "fst" `Quick test_fst
      ; test_case "snd" `Quick test_snd
      ; test_case "swap" `Quick test_swap
      ; test_case "curry" `Quick test_curry
      ; test_case "uncurry" `Quick test_uncurry
      ; test_case "&" `Quick test_and
      ; test_case "map &" `Quick test_and_sequence
      ] )
  ]
;;
