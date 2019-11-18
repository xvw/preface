open Preface.Stream
open Preface.Stream.Comonad

let rec numbers n = stream n (lazy (numbers (n + 1)))

let naturals = numbers 0

let should_extract () =
  let expected = 0 and computed = extract naturals in
  Alcotest.(check int) "should_extract" expected computed

let should_extend () =
  let increment s = 1 + extract s in
  let expected = 1 and computed = extract @@ extend increment naturals in
  Alcotest.(check int) "should_extend" expected computed

let should_infix_extend () =
  let increment s = 1 + extract s in
  let expected = 1 and computed = extract (increment <<= naturals) in
  Alcotest.(check int) "should_extend" expected computed

let should_flipped_infix_extend () =
  let increment s = 1 + extract s in
  let expected = 1 and computed = extract (naturals =>> increment) in
  Alcotest.(check int) "should_extend" expected computed

let should_compose_right_to_left () =
  let increment s = 1 + extract s in
  let to_string s = string_of_int @@ extract s in
  let expected = "1"
  and computed = compose_right_to_left to_string increment naturals in
  Alcotest.(check string) "should_compose_right_to_left" expected computed

let should_lift () =
  let increment s = 1 + s in
  let expected = 1 and computed = extract @@ lift increment naturals in
  Alcotest.(check int) "should_lift" expected computed

let should_lift2 () =
  let adder a b = a + b in
  let expected = 42
  and computed = extract @@ lift2 adder (numbers 2) (numbers 40) in
  Alcotest.(check int) "should_lift2" expected computed

let should_lift3 () =
  let adder a b c = a + b + c in
  let expected = 42
  and computed = extract @@ lift3 adder (numbers 2) (numbers 10) (numbers 30) in
  Alcotest.(check int) "should_lift3" expected computed

let should_syntax_extend () =
  let increment s = 1 + extract s in
  let expected = 1
  and computed =
    let@ x = naturals in
    increment x
  in
  Alcotest.(check int) "should_syntax_extend" expected @@ extract computed

let should_compose_left_to_right () =
  let increment s = 1 + extract s in
  let to_string s = string_of_int @@ extract s in
  let expected = "1"
  and computed = compose_left_to_right increment to_string naturals in
  Alcotest.(check string) "should_compose_left_to_right" expected computed

let should_infix_compose_right_to_left () =
  let increment s = 1 + extract s in
  let to_string s = string_of_int @@ extract s in
  let expected = "1" and computed = (to_string =<= increment) naturals in
  Alcotest.(check string) "should_infix_compose_right_to_left" expected computed

let should_infix_compose_left_to_right () =
  let increment s = 1 + extract s in
  let to_string s = string_of_int @@ extract s in
  let expected = "1" and computed = (increment =>= to_string) naturals in
  Alcotest.(check string) "should_infix_compose_left_to_right" expected computed

let should_apply () =
  let rec increments n = stream (fun x -> x + n) (lazy (increments (n + 1))) in
  let expected = 42 and computed = extract (numbers 1 <@@> increments 41) in
  Alcotest.(check int) "should_apply" expected computed

let should_inverse_apply () =
  let rec increments n = stream (fun x -> x + n) (lazy (increments (n + 1))) in
  let expected = 42 and computed = extract (increments 41 <@> numbers 1) in
  Alcotest.(check int) "should_inverse_apply" expected computed

let should_discard_first () =
  let expected = 42 and computed = extract (numbers 41 @> numbers 42) in
  Alcotest.(check int) "should_discard_first" expected computed

let should_discard_second () =
  let expected = 42 and computed = extract (numbers 42 <@ numbers 41) in
  Alcotest.(check int) "should_discard_second" expected computed

let test_cases =
  let open Alcotest in
  ( "Stream Comonad",
    [
      test_case "Extract" `Quick should_extract;
      test_case "Extend" `Quick should_extend;
      test_case "Infix Extend" `Quick should_infix_extend;
      test_case "Flipped Infix Extend" `Quick should_flipped_infix_extend;
      test_case "Compose right to left" `Quick should_compose_right_to_left;
      test_case "Lift" `Quick should_lift;
      test_case "Lift2" `Quick should_lift2;
      test_case "Lift3" `Quick should_lift3;
      test_case "Syntax Extend" `Quick should_syntax_extend;
      test_case "Compose left to right" `Quick should_compose_left_to_right;
      test_case
        "Infix Compose left to right"
        `Quick
        should_infix_compose_left_to_right;
      test_case
        "Infix Compose right to left"
        `Quick
        should_infix_compose_right_to_left;
      test_case "Apply" `Quick should_apply;
      test_case "Inverse apply" `Quick should_inverse_apply;
      test_case "Discard first" `Quick should_discard_first;
      test_case "Discard second" `Quick should_discard_second;
    ] )
