let () =
  let open Alcotest in
  run "Preface Behaviours Test"
    ( Identity.cases
    @ Option.cases
    @ Try.cases
    @ Validation.cases
    @ List.cases
    @ Nonempty_list.cases
    @ Either.cases
    @ Continuation.cases
    @ Stream.cases
    @ State.cases
    @ Misc.cases )
;;
