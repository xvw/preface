let () =
  let open Alcotest in
  run
    "Preface_stdlib"
    (Identity_test.test_cases
    @ Option_test.test_cases
    @ List_test.test_cases
    @ Try_test.test_cases
    @ Validation_test.test_cases
    @ List_applicative_traversable_option_test.test_cases
    @ List_monad_traversable_option_test.test_cases
    @ Stream_comonad_test.test_cases)
