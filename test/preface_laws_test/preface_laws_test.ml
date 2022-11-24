let run count =
  Alcotest.run "Preface Laws"
    ( Misc.cases ~count
    @ Identity.cases ~count
    @ List.cases ~count
    @ Predicate.cases ~count
    @ Equivalence.cases ~count
    @ Pair.cases ~count
    @ Fun.cases ~count )
;;

let () = run 250
