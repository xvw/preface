open Preface_test
include Preface_stdlib.List

module Requirement = struct
  type nonrec 'a t = 'a t

  let suite_name = "List"

  let arbitrary = QCheck.small_list
end

module List_functor =
  Functor_laws.Make (Functor) (Requirement) (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.String)
module List_applicative =
  Applicative_laws.Make (Applicative) (Requirement) (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
module List_monad =
  Monad_laws.Make (Monad) (Requirement) (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)

let cases = [ List_functor.cases; List_applicative.cases; List_monad.cases ]
