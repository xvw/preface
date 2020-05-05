let () =
  let open Alcotest in
  run "Preface_core" [ Fun_test.test_cases; Nonempty_list_test.test_cases ]
;;
