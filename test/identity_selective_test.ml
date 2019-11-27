module Either : Specs.EITHER = struct
  type ('a, 'b) t =
    | Left of 'a
    | Right of 'b

  let left a = Left a

  let right a = Right a

  let value_of = function
    | Left a -> a
    | Right a -> a

  let left_of = function
    | Left a -> a
    | _ -> failwith "Error: left_of waiting for a Left"

  let right_of = function
    | Right a -> a
    | _ -> failwith "Error: right_of waiting for a Right"

  let map_left f = function
    | Left a -> Left (f a)
    | Right a -> Right a

  let map_right f = function
    | Left a -> Left a
    | Right a -> Right (f a)

  let map = map_right

  let map_both f g = function
    | Left a -> Left (f a)
    | Right a -> Right (g a)
end

open Preface.Identity

open Preface.Identity.Selective (Either)

(* Material required for Alcotest *)
let identity a = Alcotest.testable (pp (Alcotest.pp a)) (eq ( = ))

let should_select_left () =
  let open Either in
  let expected = pure 42
  and computed = select (pure @@ left 40) (pure @@ ( + ) 2) in
  Alcotest.(check (identity int)) "should_select_left" expected computed

let should_select_right () =
  let open Either in
  let expected = pure 42
  and computed = select (pure @@ right 42) (pure @@ ( + ) 2) in
  Alcotest.(check (identity int)) "should_select_right" expected computed

let should_branch_left () =
  let open Either in
  let expected = pure 42
  and computed = branch (pure @@ left 40) (pure @@ ( + ) 2) (pure @@ ( + ) 4) in
  Alcotest.(check (identity int)) "should_branch_left" expected computed

let should_branch_right () =
  let open Either in
  let expected = pure 42
  and computed =
    branch (pure @@ right 40) (pure @@ ( + ) 4) (pure @@ ( + ) 2)
  in
  Alcotest.(check (identity int)) "should_branch_right" expected computed

let should_if_then_left () =
  let expected = pure 42 and computed = if_ (pure true) (pure 42) (pure 40) in
  Alcotest.(check (identity int)) "should_if_then_left" expected computed

let should_if_else_right () =
  let expected = pure 42 and computed = if_ (pure false) (pure 40) (pure 42) in
  Alcotest.(check (identity int)) "should_if_else_right" expected computed

let should_infix_select_left () =
  let open Either in
  let expected = pure 42 and computed = pure @@ left 40 <*? pure @@ ( + ) 2 in
  Alcotest.(check (identity int)) "should_infix_select_left" expected computed

let should_infix_select_right () =
  let open Either in
  let expected = pure 42 and computed = pure @@ right 42 <*? pure @@ ( + ) 2 in
  Alcotest.(check (identity int)) "should_infix_select_right" expected computed

let should_swapped_infix_select_left () =
  let open Either in
  let expected = pure 42
  and computed = (pure @@ ( + ) 2) *?> (pure @@ left 40) in
  Alcotest.(check (identity int))
    "should_swapped_infix_select_left"
    expected
    computed

let should_swapped_infix_select_right () =
  let open Either in
  let expected = pure 42
  and computed = (pure @@ ( + ) 2) *?> (pure @@ right 42) in
  Alcotest.(check (identity int))
    "should_swapped_infix_select_right"
    expected
    computed

let test_cases =
  let open Alcotest in
  ( "Selective Applicative",
    [
      test_case "Select left" `Quick should_select_left;
      test_case "Select right" `Quick should_select_right;
      test_case "Branch left" `Quick should_branch_left;
      test_case "Branch right" `Quick should_branch_right;
      test_case "If then left" `Quick should_if_then_left;
      test_case "If else right" `Quick should_if_else_right;
      test_case "Infix select left" `Quick should_infix_select_left;
      test_case "Infix select right" `Quick should_infix_select_right;
      test_case
        "Swapped infix select left"
        `Quick
        should_swapped_infix_select_left;
      test_case
        "Swapped infix select right"
        `Quick
        should_swapped_infix_select_right;
    ] )
