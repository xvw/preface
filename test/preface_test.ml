let () =
  let open Alcotest in
  run
    "Preface"
    [
      Fun_test.test_cases;
      Identity_functor_test.test_cases;
      Identity_applicative_test.test_cases;
      Identity_selective_test.test_cases;
      Identity_monad_test.test_cases;
    ]
