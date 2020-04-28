open Preface_test
include Preface_stdlib.Validation

module Requirement = struct
  type nonrec 'a t = 'a t

  let suite_name = "Validation"

  let arbitrary = Qcheck_helpers.Arbitrary.validation
end

module Validation_functor =
  Functor_laws.Make (Functor) (Requirement) (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.String)
module Validation_applicative =
  Applicative_laws.Make (Applicative) (Requirement) (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
module Validation_monad =
  Monad_laws.Make (Monad) (Requirement) (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
module Validation_selective =
  Selective_laws.Make_for_non_rigid (Selective) (Requirement)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)

let cases =
  [
    Validation_functor.cases
  ; Validation_applicative.cases
  ; Validation_monad.cases
  ; Validation_selective.cases
  ]
;;
