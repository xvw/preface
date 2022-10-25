open Preface.Identity

let identity a = Alcotest.testable (pp (Alcotest.pp a)) (equal ( = ))

let should_select_left () =
  let open Either in
  let open Selective in
  let expected = pure 42
  and computed = select (pure @@ left 40) (pure @@ ( + ) 2) in
  Alcotest.(check (identity int)) "should_select_left" expected computed
;;

let should_select_right () =
  let open Either in
  let open Selective in
  let expected = pure 42
  and computed = select (pure @@ right 42) (pure @@ ( + ) 2) in
  Alcotest.(check (identity int)) "should_select_right" expected computed
;;

let should_branch_left () =
  let open Either in
  let open Selective in
  let expected = pure 42
  and computed = branch (pure @@ left 40) (pure @@ ( + ) 2) (pure @@ ( + ) 4) in
  Alcotest.(check (identity int)) "should_branch_left" expected computed
;;

let should_branch_right () =
  let open Either in
  let open Selective in
  let expected = pure 42
  and computed =
    branch (pure @@ right 40) (pure @@ ( + ) 4) (pure @@ ( + ) 2)
  in
  Alcotest.(check (identity int)) "should_branch_right" expected computed
;;

let should_if_then_left () =
  let open Selective in
  let expected = pure 42
  and computed = if_ (pure true) (pure 42) (pure 40) in
  Alcotest.(check (identity int)) "should_if_then_left" expected computed
;;

let should_if_else_right () =
  let open Selective in
  let expected = pure 42
  and computed = if_ (pure false) (pure 40) (pure 42) in
  Alcotest.(check (identity int)) "should_if_else_right" expected computed
;;

let should_infix_select_left () =
  let open Either in
  let open Selective in
  let expected = pure 42
  and computed = pure @@ left 40 <*? pure @@ ( + ) 2 in
  Alcotest.(check (identity int)) "should_infix_select_left" expected computed
;;

let should_infix_select_right () =
  let open Either in
  let open Selective in
  let expected = pure 42
  and computed = pure @@ right 42 <*? pure @@ ( + ) 2 in
  Alcotest.(check (identity int)) "should_infix_select_right" expected computed
;;

let should_or_true_false () =
  let open Selective in
  let expected = pure true
  and computed = pure true <||> pure false in
  Alcotest.(check (identity bool)) "should_or_true_false" expected computed
;;

let should_or_false_false () =
  let open Selective in
  let expected = pure false
  and computed = pure false <||> pure false in
  Alcotest.(check (identity bool)) "should_or_false_false" expected computed
;;

let should_or_true_true () =
  let open Selective in
  let expected = pure true
  and computed = pure true <||> pure true in
  Alcotest.(check (identity bool)) "should_or_true_true" expected computed
;;

let should_or_false_true () =
  let open Selective in
  let expected = pure true
  and computed = pure false <||> pure true in
  Alcotest.(check (identity bool)) "should_or_false_true" expected computed
;;

let should_and_true_false () =
  let open Selective in
  let expected = pure false
  and computed = pure true <&&> pure false in
  Alcotest.(check (identity bool)) "should_and_true_false" expected computed
;;

let should_and_false_false () =
  let open Selective in
  let expected = pure false
  and computed = pure false <&&> pure false in
  Alcotest.(check (identity bool)) "should_and_false_false" expected computed
;;

let should_and_true_true () =
  let open Selective in
  let expected = pure true
  and computed = pure true <&&> pure true in
  Alcotest.(check (identity bool)) "should_and_true_true" expected computed
;;

let should_and_false_true () =
  let open Selective in
  let expected = pure false
  and computed = pure false <&&> pure true in
  Alcotest.(check (identity bool)) "should_and_false_true" expected computed
;;

let cases =
  let open Alcotest in
  [
    ( "Identity"
    , [
        test_case "Select left" `Quick should_select_left
      ; test_case "Select right" `Quick should_select_right
      ; test_case "Branch left" `Quick should_branch_left
      ; test_case "Branch right" `Quick should_branch_right
      ; test_case "If then left" `Quick should_if_then_left
      ; test_case "If else right" `Quick should_if_else_right
      ; test_case "Infix select left" `Quick should_infix_select_left
      ; test_case "Infix select right" `Quick should_infix_select_right
      ; test_case "Or true false" `Quick should_or_true_false
      ; test_case "Or true true" `Quick should_or_true_true
      ; test_case "Or false false" `Quick should_or_false_false
      ; test_case "Or false true" `Quick should_or_false_true
      ; test_case "And true false" `Quick should_and_true_false
      ; test_case "And true true" `Quick should_and_true_true
      ; test_case "And false false" `Quick should_and_false_false
      ; test_case "And false true" `Quick should_and_false_true
      ] )
  ]
;;
