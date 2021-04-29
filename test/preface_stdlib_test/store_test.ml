module T = Preface.Store.Over (struct
  type t = int * int
end)

let pos_test () =
  let open Alcotest in
  let expected = (23, 25)
  and computed = T.pos (T.store fst (23, 25)) in
  check (pair int int) "Should be equal" expected computed
;;

let seek_test () =
  let open Alcotest in
  let open Preface.Fun.Infix in
  let expected = (3, 6)
  and computed = (T.pos % T.seek (3, 6)) (T.store fst (23, 25)) in
  check (pair int int) "Should be equal" expected computed
;;

let seeks_test () =
  let open Alcotest in
  let open Preface.Fun.Infix in
  let expected = (25, 23)
  and computed = (T.pos % T.seeks Preface.Pair.swap) (T.store fst (23, 25)) in
  check (pair int int) "Should be equal" expected computed
;;

module Warehouse = Map.Make (Int)
module S = Preface.Store.Over (Int)

let stock =
  Warehouse.of_seq
    (List.to_seq [ (0, "OCaml"); (1, "Fsharp"); (2, "Haskell"); (3, "Scala") ])
;;

let warehouse = S.store (fun i -> Warehouse.find_opt i stock) 0

let warehouse_pos () =
  let open Alcotest in
  let computed = S.pos warehouse
  and expected = 0 in
  check int "Should be equal" expected computed
;;

let warehouse_extract () =
  let open Alcotest in
  let expected = Some "OCaml"
  and computed = S.extract warehouse in
  check (option string) "Should be equal" expected computed
;;

let warehouse_peek () =
  let open Alcotest in
  let expected = Some "Haskell"
  and computed = S.peek 2 warehouse in
  check (option string) "Should be equal" expected computed
;;

let warehouse_peeks () =
  let open Alcotest in
  let expected = Some "Fsharp"
  and computed = S.peeks succ warehouse in
  check (option string) "Should be equal" expected computed
;;

let warehouse_seek () =
  let open Alcotest in
  let expected = Some "Haskell"
  and computed = S.seek 2 warehouse |> S.extract in
  check (option string) "Should be equal" expected computed
;;

let warehouse_seeks () =
  let open Alcotest in
  let expected = Some "Fsharp"
  and computed = S.seeks succ warehouse |> S.extract in
  check (option string) "Should be equal" expected computed
;;

let warehouse_experiment () =
  let open Alcotest in
  let module F = S.Experiment (Preface.List.Functor) in
  let expected =
    [ Some "OCaml"; Some "Fsharp"; Some "Haskell"; Some "Scala"; None ]
  in
  let computed = F.run (fun x -> [ x; x + 1; x + 2; x + 3; x + 4 ]) warehouse in
  check (list (option string)) "Should be equal" expected computed
;;

let cases =
  let open Alcotest in
  [
    ( "Store"
    , [
        test_case "Test for pos" `Quick pos_test
      ; test_case "Test for seek" `Quick seek_test
      ; test_case "Test for seeks" `Quick seeks_test
      ; test_case "Test for warehouse pos" `Quick warehouse_pos
      ; test_case "Test for warehouse extract" `Quick warehouse_extract
      ; test_case "Test for warehouse peek" `Quick warehouse_peek
      ; test_case "Test for warehouse peeks" `Quick warehouse_peeks
      ; test_case "Test for warehouse seek" `Quick warehouse_seek
      ; test_case "Test for warehouse seeks" `Quick warehouse_seeks
      ; test_case "Test for warehouse experiment" `Quick warehouse_experiment
      ] )
  ]
;;
