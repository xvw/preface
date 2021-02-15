let () =
  Alcotest.(run "Preface_core" [ Nonempty_list_test.cases; Fun_test.cases ])
;;
