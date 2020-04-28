open Preface_test
include Preface_stdlib.Option

module Requirement = struct
  type nonrec 'a t = 'a t

  let suite_name = "Option"

  let arbitrary = QCheck.option
end

module Option_functor =
  Functor_laws.Make (Functor) (Requirement) (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.String)
module Option_applicative =
  Applicative_laws.Make (Applicative) (Requirement) (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
module Option_monad =
  Monad_laws.Make (Monad) (Requirement) (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)

let cases =
  [ Option_functor.cases; Option_applicative.cases; Option_monad.cases ]
;;
