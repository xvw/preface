let () =
  let open Alcotest in
  run "Preface laws"
    ( Identity_test.cases
    @ Option_test.cases
    @ Try_test.cases
    @ Validation_test.cases
    @ List_test.cases
    @ Nonempty_list_test.cases
    @ Either_test.cases
    @ Continuation_test.cases
    @ Stream_test.cases
    @ State_test.cases )
;;
