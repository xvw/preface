let () =
  let open Alcotest in
  run "Preface" [Fun_test.test_cases; Identity_test.test_cases]
