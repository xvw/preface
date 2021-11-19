let () =
  let open Alcotest in
  run "Preface examples"
    ( Arrow_tutorial.cases
    @ Formlet.cases
    @ Free_applicative_formlet.cases
    @ Shape.cases
    @ Free_monad_consoleIO.cases
    @ Free_monad_consoleIO_composition.cases
    @ Freer_monad_consoleIO.cases
    @ Freer_monad_os_effect.cases
    @ Freer_monad_os_explicit_continuation.cases
    @ Free_selective_pingpong.cases
    @ Freer_selective_ping_pong.cases
    @ Template_reader.cases
    @ Debruijn_reader.cases
    @ Approximation_for_selective.cases
    @ Xml_stax_writer.cases
    @ Traced_dependencies.cases )
;;
