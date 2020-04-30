open Preface_test
include Preface_stdlib.Either

module Requirement (T : Preface_specs.Types.T0) = struct
  type nonrec 'a t = (T.t, 'a) t

  let suite_name = "Either (with int as Left)"

  let arbitrary right = Qcheck_helpers.Arbitrary.either QCheck.int right
end

module T_int = struct
  type t = int
end

module Either_int_functor =
  Functor_laws.Make
    (Functor (T_int)) (Requirement (T_int)) (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.String)
module Either_int_applicative =
  Applicative_laws.Make
    (Applicative (T_int)) (Requirement (T_int)) (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
module Either_int_monad =
  Monad_laws.Make
    (Monad (T_int)) (Requirement (T_int)) (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)

module Either_bifunctor =
  Bifunctor_laws.Make
    (Bifunctor)
    (struct
      let suite_name = "Either"

      type ('a, 'b) t = ('a, 'b) Bifunctor.t

      let arbitrary = Qcheck_helpers.Arbitrary.either
    end)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)

let cases =
  [
    Either_int_functor.cases
  ; Either_int_applicative.cases
  ; Either_int_monad.cases
  ; Either_bifunctor.cases
  ]
;;
