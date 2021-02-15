let run number =
  let open Alcotest in
  run "Preface_laws"
    ( Misc.cases number
    @ Identity.cases number
    @ Continuation.cases number
    @ List.cases number
    @ Nonempty_list.cases number
    @ Option.cases number
    @ Result.cases number
    @ Try.cases number
    @ Validation.cases number
    @ Validate.cases number
    @ Stream.cases number
    @ Either.cases number
    @ State.cases number
    @ Predicate.cases number )
;;

let () = run 500
