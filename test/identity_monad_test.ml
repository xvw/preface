open Preface.Identity
open Preface.Identity.Monad

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

let should_bind_with_infix_operator () =
  let expected = return 42
  and computed = return 40 >>= fun x -> return (x + 2) in
  Alcotest.(check (identity int))
    "should_bind_with_infix_operator"
    expected
    computed

let test_cases =
  let open Alcotest in
  ( "Identity Monad"
  , [ test_case "Bind" `Quick should_bind
    ; test_case "Map" `Quick should_map
    ; test_case "Join" `Quick should_join
    ; test_case "Compose left to right" `Quick should_compose_left_to_right
    ; test_case "Void" `Quick should_void
    ; test_case "Compose right to left" `Quick should_compose_right_to_left
    ; test_case "Infix Bind" `Quick should_bind_with_infix_operator ] )
