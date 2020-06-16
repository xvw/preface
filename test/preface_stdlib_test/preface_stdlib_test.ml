let () =
  let open Alcotest in
  run "Preface_stdlib"
    ( Identity_test.test_cases
    @ Option_test.test_cases
    @ List_test.test_cases
    @ Try_test.test_cases
    @ State_test.test_cases
    @ Validation_test.test_cases
    @ Validation_nel_test.test_cases
    @ List_applicative_traversable_option_test.test_cases
    @ List_monad_traversable_option_test.test_cases
    @ Stream_test.test_cases
    @ Continuation_test.test_cases
    @ Free_monad_test.test_cases
    @ Freer_monad_test.test_cases
    @ Semigroup_test.test_cases
    @ Monoid_test.test_cases
    @ Arrow_test.test_cases
    @ Reader_test.test_cases )
;;
