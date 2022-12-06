let run count =
  Alcotest.run "Preface Laws"
    ( Misc.cases ~count
    @ Approximation.cases ~count
    @ Identity.cases ~count
    @ Option.cases ~count
    @ Either.cases ~count
    @ Pair.cases ~count
    @ List.cases ~count
    @ Nonempty_list.cases ~count
    @ Seq.cases ~count
    @ Stream.cases ~count
    @ Result.cases ~count
    @ Validation.cases ~count
    @ Try.cases ~count
    @ Validate.cases ~count
    @ Fun.cases ~count
    @ Predicate.cases ~count
    @ Equivalence.cases ~count
    @ Continuation.cases ~count )
;;

let () = run 250
