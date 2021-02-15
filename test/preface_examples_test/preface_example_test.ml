let () =
  let open Alcotest in
  run "Preface examples"
    ( Arrow_tutorial.cases
    @ Formlet.cases
    @ Shape.cases
    @ Free_monad_consoleIO.cases
    @ Freer_monad_consoleIO.cases
    @ Template_reader.cases
    @ Debruijn_reader.cases )
;;
