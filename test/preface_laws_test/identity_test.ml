open Preface_test
include Preface_stdlib.Identity

module Requirement = struct
  type nonrec 'a t = 'a t

  let suite_name = "Identity"

  let arbitrary = Qcheck_helpers.Arbitrary.identity
end

module Identity_functor =
  Functor_laws.Make (Functor) (Requirement) (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.String)
module Identity_applicative =
  Applicative_laws.Make (Applicative) (Requirement) (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
module Identity_monad =
  Monad_laws.Make (Monad) (Requirement) (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
module Identity_selective =
  Selective_laws.Make_for_rigid (Selective) (Requirement)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)

let cases =
  [
    Identity_functor.cases
  ; Identity_applicative.cases
  ; Identity_monad.cases
  ; Identity_selective.cases
  ]
;;
