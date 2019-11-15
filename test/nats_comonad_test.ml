open Preface.Stream
open Preface.Stream.Comonad

let nats =
  let rec nats n = stream n (lazy (nats (n + 1))) in
  nats 0

let should_extract () =
  let expected = 0 and computed = extract nats in
  Alcotest.(check int) "should_extract" expected computed

let should_extend () =
  let increment s = 1 + extract s in
  let expected = 1 and computed = extract @@ extend increment nats in
  Alcotest.(check int) "should_extend" expected computed

let should_infix_extend () =
  let increment s = 1 + extract s in
  let expected = 1 and computed = extract (increment <<= nats) in
  Alcotest.(check int) "should_extend" expected computed

let should_flipped_infix_extend () =
  let increment s = 1 + extract s in
  let expected = 1 and computed = extract (nats =>> increment) in
  Alcotest.(check int) "should_extend" expected computed

let should_compose_left_to_right () =
  let increment s = 1 + extract s in
  let to_string s = string_of_int @@ extract s in
  let expected = "1"
  and computed = compose_left_to_right increment to_string nats in
  Alcotest.(check string) "should_compose_left_to_right" expected computed

let should_compose_right_to_left () =
  let increment s = 1 + extract s in
  let to_string s = string_of_int @@ extract s in
  let expected = "1"
  and computed = compose_right_to_left to_string increment nats in
  Alcotest.(check string) "should_compose_right_to_left" expected computed

let should_infix_compose_left_to_right () =
  let increment s = 1 + extract s in
  let to_string s = string_of_int @@ extract s in
  let expected = "1" and computed = (increment =>= to_string) nats in
  Alcotest.(check string) "should_infix_compose_left_to_right" expected computed

let should_infix_compose_right_to_left () =
  let increment s = 1 + extract s in
  let to_string s = string_of_int @@ extract s in
  let expected = "1" and computed = (to_string =<= increment) nats in
  Alcotest.(check string) "should_infix_compose_right_to_left" expected computed

let test_cases =
  let open Alcotest in
  ( "Stream Comonad"
  , [ test_case "Extract" `Quick should_extract
    ; test_case "Extend" `Quick should_extend
    ; test_case "Infix Extend" `Quick should_infix_extend
    ; test_case "Flipped Infix Extend" `Quick should_flipped_infix_extend
    ; test_case "Compose left to right" `Quick should_compose_left_to_right
    ; test_case "Compose right to left" `Quick should_compose_right_to_left
    ; test_case
        "Infix Compose left to right"
        `Quick
        should_infix_compose_left_to_right
    ; test_case
        "Infix Compose right to left"
        `Quick
        should_infix_compose_right_to_left ] )
