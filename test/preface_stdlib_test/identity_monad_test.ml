open Preface_stdlib.Identity
open Preface_stdlib.Identity.Monad

(* Material required for Alcotest *)
let identity a = Alcotest.testable (pp @@ Alcotest.pp a) @@ eq ( = )

let should_bind () =
  let expected = return 42
  and computed = bind (fun x -> return (x + 2)) @@ return 40 in
  Alcotest.(check (identity int)) "should_bind" expected computed

let should_map () =
  let expected = return 42 and computed = map (( + ) 2) @@ return 40 in
  Alcotest.(check (identity int)) "should_map" expected computed

let should_join () =
  let expected = return 42 and computed = join @@ return (return 42) in
  Alcotest.(check (identity int)) "should_join" expected computed

let should_compose_left_to_right () =
  let expected = return "42"
  and computed =
    compose_left_to_right
      (fun x -> return (x + 2))
      (fun x -> return (string_of_int x))
      40
  in
  Alcotest.(check (identity string))
    "should_compose_left_to_right"
    expected
    computed

let should_void () =
  let expected = return () and computed = void @@ return 42 in
  Alcotest.(check (identity unit)) "should_void" expected computed

let should_compose_right_to_left () =
  let expected = return "42"
  and computed =
    compose_right_to_left
      (fun x -> return (string_of_int x))
      (fun x -> return (x + 2))
      40
  in
  Alcotest.(check (identity string))
    "should_compose_right_to_left"
    expected
    computed

let should_lift () =
  let expected = return 42 and computed = lift (( + ) 2) @@ return 40 in
  Alcotest.(check (identity int)) "should_lift" expected computed

let should_lift2 () =
  let expected = return 42 and computed = lift2 ( + ) (return 40) (return 2) in
  Alcotest.(check (identity int)) "should_lift2" expected computed

let should_lift3 () =
  let add a b c = a + b + c in
  let expected = pure 42 and computed = lift3 add (pure 36) (pure 4) (pure 2) in
  Alcotest.(check (identity int)) "should_lift3" expected computed

let should_flipped_bind_with_syntax () =
  let expected = pure 42
  and computed =
    let* x = pure 2 in
    pure (x + 40)
  in
  Alcotest.(check (identity int))
    "should_flipped_bind_with_syntax"
    expected
    computed

let should_map_with_infix_operator () =
  let expected = return 42 and computed = ( + ) 2 =|< return 40 in
  Alcotest.(check (identity int))
    "should_map_with_infix_operator"
    expected
    computed

let should_flipped_map_with_infix_operator () =
  let expected = return 42 and computed = return 40 >|= ( + ) 2 in
  Alcotest.(check (identity int))
    "should_flipped_map_with_infix_operator"
    expected
    computed

let should_flipped_bind_with_infix_operator () =
  let expected = return 42
  and computed = return 40 >>= fun x -> return (x + 2) in
  Alcotest.(check (identity int))
    "should_flipped_bind_with_infix_operator"
    expected
    computed

let should_bind_with_infix_operator () =
  let expected = return 42
  and computed = (fun x -> return (x + 2)) =<< return 40 in
  Alcotest.(check (identity int))
    "should_bind_with_infix_operator"
    expected
    computed

let should_compose_left_to_right_with_infix_operator () =
  let expected = return "42"
  and computed =
    ((fun x -> return (x + 2)) >=> fun x -> return (string_of_int x)) 40
  in
  Alcotest.(check (identity string))
    "should_compose_left_to_right_with_infix_operator"
    expected
    computed

let should_compose_right_to_left_with_infix_operator () =
  let expected = return "42"
  and computed =
    ((fun x -> return (string_of_int x)) <=< fun x -> return (x + 2)) 40
  in
  Alcotest.(check (identity string))
    "should_compose_left_to_right_with_infix_operator"
    expected
    computed

let should_discard_first_value () =
  let expected = return 42 and computed = return "42" >> return 42 in
  Alcotest.(check (identity int)) "should_discard_first_value" expected computed

let should_discard_second_value () =
  let expected = return 42 and computed = return 42 << return "42" in
  Alcotest.(check (identity int))
    "should_discard_second_value"
    expected
    computed

let test_cases =
  let open Alcotest in
  ( "Identity Monad",
    [
      test_case "Bind" `Quick should_bind;
      test_case "Map" `Quick should_map;
      test_case "Join" `Quick should_join;
      test_case "Compose left to right" `Quick should_compose_left_to_right;
      test_case "Void" `Quick should_void;
      test_case "Compose right to left" `Quick should_compose_right_to_left;
      test_case "Infix Bind" `Quick should_bind_with_infix_operator;
      test_case "Lift" `Quick should_lift;
      test_case "Lift2" `Quick should_lift2;
      test_case "Lift3" `Quick should_lift3;
      test_case
        "Flipped Bind with syntax"
        `Quick
        should_flipped_bind_with_syntax;
      test_case "Map with infix operator" `Quick should_map_with_infix_operator;
      test_case
        "Flipped Map with infix operator"
        `Quick
        should_flipped_map_with_infix_operator;
      test_case
        "Flipped Bind with infix operator"
        `Quick
        should_flipped_bind_with_infix_operator;
      test_case
        "Bind with infix operator"
        `Quick
        should_bind_with_infix_operator;
      test_case
        "Compose Left to Right with infix operator"
        `Quick
        should_compose_left_to_right_with_infix_operator;
      test_case
        "Compose Right to Left with infix operator"
        `Quick
        should_compose_right_to_left_with_infix_operator;
      test_case "Discard First value" `Quick should_discard_first_value;
      test_case "Discard Second value" `Quick should_discard_second_value;
    ] )
