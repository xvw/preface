open Preface_stdlib.List

open Monad.Traversable (Preface_stdlib.Option.Monad)

let should_traverse_without_failure () =
  let expected = Some [ 1; 2; 3 ]
  and computed = sequence [ Some 1; Some 2; Some 3 ] in
  Alcotest.(check (option (list int)))
    "should_traverse_without_failure"
    expected
    computed

let should_traverse_with_failure () =
  let expected = None and computed = sequence [ Some 1; None; Some 3 ] in
  Alcotest.(check (option (list int)))
    "should_traverse_with_failure"
    expected
    computed

let test_cases =
  let open Alcotest in
  [
    ( "List Traversable for Option as a Monad",
      [
        test_case
          "Sequence with valid input"
          `Quick
          should_traverse_without_failure;
        test_case
          "Sequence with invalid input"
          `Quick
          should_traverse_with_failure;
      ] );
  ]
