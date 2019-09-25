open Preface.Identity
open Preface.Identity.Functor
open Preface.Identity.Applicative

(* Material required for Alcotest *)
let identity a = Alcotest.testable (pp @@ Alcotest.pp a) @@ eq ( = )

let should_map () =
  let expected = pure 42 and computed = map (( + ) 2) @@ pure 40 in
  Alcotest.(check (identity int)) "should_map" expected computed

let should_product_first () =
  let expected = pure 42 and computed = fst <$> product (pure 42) (pure 2) in
  Alcotest.(check (identity int)) "should_product_first" expected computed

let should_product_second () =
  let expected = pure 42 and computed = snd <$> product (pure 2) (pure 42) in
  Alcotest.(check (identity int)) "should_product_second" expected computed

let should_apply () =
  let expected = pure 42 and computed = apply (pure @@ ( + ) 2) @@ pure 40 in
  Alcotest.(check (identity int)) "should_apply" expected computed

let should_lift () =
  let expected = pure 42 and computed = lift (( + ) 2) (pure 40) in
  Alcotest.(check (identity int)) "should_ lift" expected computed

let should_lift2 () =
  let expected = pure 42 and computed = lift2 ( + ) (pure 40) (pure 2) in
  Alcotest.(check (identity int)) "should_ lift2" expected computed

let should_lift3 () =
  let add a b c = a + b + c in
  let expected = pure 42
  and computed = lift3 add (pure 36) (pure 4) (pure 2) in
  Alcotest.(check (identity int)) "should_ lift3" expected computed

let should_map_with_syntax () =
  let expected = pure 42
  and computed =
    let+ x = pure 40 in
    x + 2
  in
  Alcotest.(check (identity int)) "should_map" expected computed

let should_map_and_product_with_syntax () =
  let expected = pure 42
  and computed =
    let+ f = pure ( + ) and+ x = pure 40 and+ y = pure 2 in
    f x y
  in
  Alcotest.(check (identity int)) "should_map" expected computed

let should_apply_with_infix_operator () =
  let expected = pure 42 and computed = ( + ) <$> pure 40 <*> pure 2 in
  Alcotest.(check (identity int))
    "should_apply_with_infix_operator"
    expected
    computed

let should_flipped_apply_with_infix_operator () =
  let expected = pure 42 and computed = pure 40 <**> pure (( + ) 2) in
  Alcotest.(check (identity int))
    "should_flipped_apply_with_infix_operator"
    expected
    computed

let should_discard_with_infix_operator () =
  let expected = pure 42 and computed = pure 2 *> pure 42 in
  Alcotest.(check (identity int))
    "should_discard_with_infix_operator"
    expected
    computed

let should_flipped_discard_with_infix_operator () =
  let expected = pure 42 and computed = pure 42 <* pure 2 in
  Alcotest.(check (identity int))
    "should_flipped_apply_with_infix_operator"
    expected
    computed

let test_cases =
  let open Alcotest in
  ( "Identity Applicative"
  , [ test_case "Map" `Quick should_map
    ; test_case "First of product" `Quick should_product_first
    ; test_case "Second of product" `Quick should_product_second
    ; test_case "Apply" `Quick should_apply
    ; test_case "Lift" `Quick should_lift
    ; test_case "Lift2" `Quick should_lift2
    ; test_case "Lift3" `Quick should_lift3
    ; test_case "Map using syntax" `Quick should_map_with_syntax
    ; test_case
        "Map and Product using syntax"
        `Quick
        should_map_and_product_with_syntax
    ; test_case "Infix Apply" `Quick should_apply_with_infix_operator
    ; test_case
        "Infix Flipped Apply"
        `Quick
        should_flipped_apply_with_infix_operator
    ; test_case "Infix Discard" `Quick should_discard_with_infix_operator
    ; test_case
        "Infix Flipped Discard"
        `Quick
        should_flipped_discard_with_infix_operator ] )
