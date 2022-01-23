let seq t =
  Alcotest.testable
    (Preface.Seq.pp (Alcotest.pp t))
    (Preface.Seq.equal (Alcotest.equal t))
;;

let fold_map_over_values () =
  let module Prod = Preface.Make.Monoid.Via_combine_and_neutral (struct
    type t = int

    let neutral = 1
    let combine = ( * )
  end) in
  let expected = 120
  and computed =
    Preface.Seq.Foldable.fold_map
      (module Prod)
      int_of_string
      (List.to_seq [ "1"; "2"; "3"; "4"; "5" ])
  in
  Alcotest.(check int) "fold_map with success" expected computed
;;

let fold_map_over_empty () =
  let module Prod = Preface.Make.Monoid.Via_combine_and_neutral (struct
    type t = int

    let neutral = 1
    let combine = ( * )
  end) in
  let expected = 1
  and computed =
    Preface.Seq.Foldable.fold_map (module Prod) int_of_string Seq.empty
  in
  Alcotest.(check int) "fold_map with success" expected computed
;;

let should_traverse_without_failure_applicative () =
  let open
    Preface.Seq.Applicative.Traversable (Preface_stdlib.Option.Applicative) in
  let expected = Some (List.to_seq [ 1; 2; 3 ])
  and computed = sequence (List.to_seq [ Some 1; Some 2; Some 3 ]) in
  Alcotest.(check (option (seq int)))
    "should_traverse_without_failure_applicative" expected computed
;;

let should_traverse_with_failure_applicative () =
  let open
    Preface.Seq.Applicative.Traversable (Preface_stdlib.Option.Applicative) in
  let expected = None
  and computed = sequence (List.to_seq [ Some 1; None; Some 3 ]) in
  Alcotest.(check (option (seq int)))
    "should_traverse_with_failure_applicative" expected computed
;;

let should_traverse_without_failure_monad () =
  let open Preface.Seq.Monad.Traversable (Preface_stdlib.Option.Monad) in
  let expected = Some (List.to_seq [ 1; 2; 3 ])
  and computed = sequence (List.to_seq [ Some 1; Some 2; Some 3 ]) in
  Alcotest.(check (option (seq int)))
    "should_traverse_without_failure_monad" expected computed
;;

let should_traverse_with_failure_monad () =
  let open Preface.Seq.Monad.Traversable (Preface_stdlib.Option.Monad) in
  let expected = None
  and computed = sequence (List.to_seq [ Some 1; None; Some 3 ]) in
  Alcotest.(check (option (seq int)))
    "should_traverse_with_failure_monad" expected computed
;;

let cases =
  let open Alcotest in
  [
    ( "Seq"
    , [
        test_case "Fold_map over values" `Quick fold_map_over_values
      ; test_case "Fold_map over empty" `Quick fold_map_over_empty
      ; test_case "Sequence with valid input and applicative" `Quick
          should_traverse_without_failure_applicative
      ; test_case "Sequence with invalid input and applicative" `Quick
          should_traverse_with_failure_applicative
      ; test_case "Sequence with valid input with monad" `Quick
          should_traverse_without_failure_monad
      ; test_case "Sequence with invalid input with monad" `Quick
          should_traverse_with_failure_monad
      ] )
  ]
;;
