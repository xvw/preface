open Preface_stdlib.Stream
open Preface_stdlib.Stream.Comonad
module Try = Preface_stdlib.Try

let subject_try a = Alcotest.testable (Try.pp (Alcotest.pp a)) (Try.equal ( = ))

let rec numbers n = stream n (lazy (numbers (n + 1)))

let naturals = numbers 0

let should_extract () =
  let expected = 0
  and computed = extract naturals in
  Alcotest.(check int) "should_extract" expected computed
;;

let should_extend () =
  let increment s = 1 + extract s in
  let expected = 1
  and computed = extract @@ extend increment naturals in
  Alcotest.(check int) "should_extend" expected computed
;;

let should_infix_extend () =
  let increment s = 1 + extract s in
  let expected = 1
  and computed = extract (increment <<= naturals) in
  Alcotest.(check int) "should_extend" expected computed
;;

let should_flipped_infix_extend () =
  let increment s = 1 + extract s in
  let expected = 1
  and computed = extract (naturals =>> increment) in
  Alcotest.(check int) "should_extend" expected computed
;;

let should_compose_right_to_left () =
  let increment s = 1 + extract s in
  let to_string s = string_of_int @@ extract s in
  let expected = "1"
  and computed = compose_right_to_left to_string increment naturals in
  Alcotest.(check string) "should_compose_right_to_left" expected computed
;;

let should_lift () =
  let increment s = 1 + s in
  let expected = 1
  and computed = extract @@ lift increment naturals in
  Alcotest.(check int) "should_lift" expected computed
;;

let should_lift2 () =
  let adder a b = a + b in
  let expected = 42
  and computed = extract @@ lift2 adder (numbers 2) (numbers 40) in
  Alcotest.(check int) "should_lift2" expected computed
;;

let should_lift3 () =
  let adder a b c = a + b + c in
  let expected = 42
  and computed = extract @@ lift3 adder (numbers 2) (numbers 10) (numbers 30) in
  Alcotest.(check int) "should_lift3" expected computed
;;

let should_syntax_extend () =
  let increment s = 1 + extract s in
  let expected = 1
  and computed =
    let@ x = naturals in
    increment x
  in
  Alcotest.(check int) "should_syntax_extend" expected @@ extract computed
;;

let should_compose_left_to_right () =
  let increment s = 1 + extract s in
  let to_string s = string_of_int @@ extract s in
  let expected = "1"
  and computed = compose_left_to_right increment to_string naturals in
  Alcotest.(check string) "should_compose_left_to_right" expected computed
;;

let should_infix_compose_right_to_left () =
  let increment s = 1 + extract s in
  let to_string s = string_of_int @@ extract s in
  let expected = "1"
  and computed = (to_string =<= increment) naturals in
  Alcotest.(check string) "should_infix_compose_right_to_left" expected computed
;;

let should_infix_compose_left_to_right () =
  let increment s = 1 + extract s in
  let to_string s = string_of_int @@ extract s in
  let expected = "1"
  and computed = (increment =>= to_string) naturals in
  Alcotest.(check string) "should_infix_compose_left_to_right" expected computed
;;

let should_apply () =
  let rec increments n = stream (fun x -> x + n) (lazy (increments (n + 1))) in
  let expected = 42
  and computed = extract (numbers 1 <@@> increments 41) in
  Alcotest.(check int) "should_apply" expected computed
;;

let should_inverse_apply () =
  let rec increments n = stream (fun x -> x + n) (lazy (increments (n + 1))) in
  let expected = 42
  and computed = extract (increments 41 <@> numbers 1) in
  Alcotest.(check int) "should_inverse_apply" expected computed
;;

let should_discard_first () =
  let expected = 42
  and computed = extract (pure () @> numbers 42) in
  Alcotest.(check int) "should_discard_first" expected computed
;;

let should_discard_second () =
  let expected = 42
  and computed = extract (numbers 42 <@ pure ()) in
  Alcotest.(check int) "should_discard_second" expected computed
;;

let hd_get_a_value () =
  let expected = 42
  and computed = hd (numbers 42) in
  Alcotest.(check int) "should_extract_head" expected computed
;;

let tl_get_a_stream () =
  let expected = 43
  and computed = hd (tl (numbers 42)) in
  Alcotest.(check int) "should_extract_head_of_tail" expected computed
;;

let cons_test () =
  let expected = 68
  and expected2 = 0
  and computed = hd (68 <:> naturals)
  and computed2 = hd (tl (68 <:> naturals)) in
  Alcotest.(check int) "should_extract_head_of_cons" expected computed;
  Alcotest.(check int) "should_extract_head_of_cons" expected2 computed2
;;

let access_test () =
  let expected = Try.ok 1000
  and computed = naturals.%[1000] in
  Alcotest.(check (subject_try int)) "should_access" expected computed
;;

let access_test_2 () =
  let expected = Try.error (Preface_stdlib.Exn.Negative_position (-10))
  and computed = naturals.%[-10] in
  Alcotest.(check (subject_try int)) "access_should_fail" expected computed
;;

let fibonacci () =
  let rec fb a b = stream (a + b) (lazy (fb b (a + b))) in
  let fib = 0 <:> stream 1 (lazy (fb 0 1)) in
  Alcotest.(check (subject_try int)) "fibonacci 0" (Try.ok 0) fib.%[0];
  Alcotest.(check (subject_try int)) "fibonacci 2" (Try.ok 1) fib.%[2];
  Alcotest.(check (subject_try int)) "fibonacci 6" (Try.ok 8) fib.%[6];
  Alcotest.(check (subject_try int)) "fibonacci 16" (Try.ok 987) fib.%[16]
;;

let drop_take_test () =
  let open Try.Monad.Infix in
  let expected = Try.ok [ 4; 5; 6; 7 ]
  and computed = drop 4 naturals >>= take 4 in
  Alcotest.(check (subject_try (list int)))
    "access_drop_and_take" expected computed
;;

let take_while_test () =
  let expected = [ 0; 1; 2; 3; 4 ]
  and computed = take_while (fun x -> x < 5) naturals in
  Alcotest.(check (list int)) "should_take_while" expected computed
;;

let drop_while_test () =
  let expected = Try.ok [ 5; 6; 7; 8; 9; 10 ]
  and computed = drop_while (fun x -> x < 5) naturals |> take 6 in
  Alcotest.(check (subject_try (list int)))
    "should_drop_while" expected computed
;;

let test_cases =
  let open Alcotest in
  [
    ( "Stream"
    , [
        test_case "Extract" `Quick should_extract
      ; test_case "Extend" `Quick should_extend
      ; test_case "Infix Extend" `Quick should_infix_extend
      ; test_case "Flipped Infix Extend" `Quick should_flipped_infix_extend
      ; test_case "Compose right to left" `Quick should_compose_right_to_left
      ; test_case "Lift" `Quick should_lift
      ; test_case "Lift2" `Quick should_lift2
      ; test_case "Lift3" `Quick should_lift3
      ; test_case "Syntax Extend" `Quick should_syntax_extend
      ; test_case "Compose left to right" `Quick should_compose_left_to_right
      ; test_case "Infix Compose left to right" `Quick
          should_infix_compose_left_to_right
      ; test_case "Infix Compose right to left" `Quick
          should_infix_compose_right_to_left
      ; test_case "Apply" `Quick should_apply
      ; test_case "Inverse apply" `Quick should_inverse_apply
      ; test_case "Discard first" `Quick should_discard_first
      ; test_case "Discard second" `Quick should_discard_second
      ; test_case "Get head" `Quick hd_get_a_value
      ; test_case "Get head of tail" `Quick tl_get_a_stream
      ; test_case "Cons" `Quick cons_test
      ; test_case "Access with valid offset" `Quick access_test
      ; test_case "Access with invalid offset" `Quick access_test_2
      ; test_case "Get fibonacci numbers" `Quick fibonacci
      ; test_case "Drop and take" `Quick drop_take_test
      ; test_case "Takewhile" `Quick take_while_test
      ; test_case "Dropwhile" `Quick drop_while_test
      ] )
  ]
;;
