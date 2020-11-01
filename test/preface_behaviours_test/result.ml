open Preface_stdlib.Result

module Requirement (T : Preface_specs.Types.T0) = struct
  type nonrec 'a t = ('a, T.t) t

  let name = "Result (with int as Error)"

  let size = 100

  let arbitrary ok = Preface_qcheck.Arbitrary.result ok QCheck.int
end

module Requirement2 = struct
  type nonrec ('a, 'b) t = ('a, 'b) t

  let name = "Result"

  let size = 100

  let arbitrary left right = Preface_qcheck.Arbitrary.result left right
end

module T_int = struct
  type t = int
end

module Functor_test =
  Preface_qcheck.Functor.Make
    (Functor (T_int)) (Requirement (T_int)) (Preface_qcheck.Sample.Pack)
module Applicative_test =
  Preface_qcheck.Applicative.Make
    (Applicative (T_int)) (Requirement (T_int)) (Preface_qcheck.Sample.Pack)
module Monad_test =
  Preface_qcheck.Monad.Make
    (Monad (T_int)) (Requirement (T_int)) (Preface_qcheck.Sample.Pack)
module Bifunctor_test =
  Preface_qcheck.Bifunctor.Make (Bifunctor) (Requirement2)
    (Preface_qcheck.Sample.Pack)

let cases =
  Functor_test.cases
  @ Applicative_test.cases
  @ Monad_test.cases
  @ Bifunctor_test.cases
;;
