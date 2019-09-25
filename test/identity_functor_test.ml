open Preface.Identity
open Preface.Identity.Functor

(* Material required for Alcotest *)
let identity a = Alcotest.testable (pp (Alcotest.pp a)) (eq ( = ))

let should_replace () =
  let expected = pure 42 and computed = replace 42 (pure 41) in
  Alcotest.(check (identity int)) "should_replace" expected computed

let should_replace_with_infix_operator () =
  let expected = pure 42 and computed = 42 <$ pure 41 in
  Alcotest.(check (identity int))
    "should_replace_with_infix_operator"
    expected
    computed

let should_flipped_replace_with_infix_operator () =
  let expected = pure 42 and computed = pure 41 $> 42 in
  Alcotest.(check (identity int))
    "should_flipped_replace_with_infix_operator"
    expected
    computed

let should_map () =
  let expected = pure 42 and computed = map (( + ) 1) (pure 41) in
  Alcotest.(check (identity int)) "should_map" expected computed

let should_void () =
  let expected = pure 42
  and computed =
    map (fun _ -> 42) (void (pure 41))
    (* void testable does not exist (yet!) *)
  in
  Alcotest.(check (identity int)) "should_void" expected computed

let should_map_with_infix_operator () =
  let expected = pure 42 and computed = ( + ) 1 <$> pure 41 in
  Alcotest.(check (identity int))
    "should_map_with_infix_operator"
    expected
    computed

let should_flipped_map_with_infix_operator () =
  let expected = pure 42 and computed = pure 41 <&> ( + ) 1 in
  Alcotest.(check (identity int))
    "should_flipped_map_with_infix_operator"
    expected
    computed

let test_cases =
  let open Alcotest in
  ( "Identity Functor"
  , [ test_case "Replace" `Quick should_replace
    ; test_case "Infix replace" `Quick should_replace_with_infix_operator
    ; test_case
        "Infix flipped replace"
        `Quick
        should_flipped_replace_with_infix_operator
    ; test_case "Void" `Quick should_void
    ; test_case "Map" `Quick should_map
    ; test_case "Infix map" `Quick should_map_with_infix_operator
    ; test_case
        "Infix flipped map"
        `Quick
        should_flipped_map_with_infix_operator ] )
