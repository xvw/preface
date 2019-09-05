let () =
  let open Alcotest in
  run "Preface" [Identity_test.test_cases]
