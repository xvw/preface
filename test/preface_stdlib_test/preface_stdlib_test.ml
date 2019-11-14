let () =
  let open Alcotest in
  run
    "Preface_stdlib"
    [
      Identity_functor_test.test_cases;
      Identity_applicative_test.test_cases;
      Identity_monad_test.test_cases;
    ]