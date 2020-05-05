open Preface_test
include Preface_stdlib.Nonempty_list

module Requirement = struct
  type nonrec 'a t = 'a t

  let suite_name = "Nonempty_list"

  let arbitrary x = Qcheck_helpers.Arbitrary.nonempty_list x
end

module Nonempty_list_functor =
  Functor_laws.Make (Functor) (Requirement) (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.String)
module Nonempty_list_applicative =
  Applicative_laws.Make (Applicative) (Requirement) (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
module Nonempty_list_monad =
  Monad_laws.Make (Monad) (Requirement) (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
module Nonempty_list_selective =
  Selective_laws.Make_for_rigid (Selective) (Requirement)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)

let cases =
  let open List in
  [
    Nonempty_list_functor.cases
  ; Nonempty_list_applicative.cases
  ; Nonempty_list_monad.cases
  ; Nonempty_list_selective.cases
  ]
;;
