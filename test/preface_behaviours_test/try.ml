open Preface_stdlib.Try

module Requirement = struct
  type nonrec 'a t = 'a t

  let name = "Try"

  let size = 100

  let arbitrary x = Preface_qcheck.Arbitrary.try_ x
end

module Functor_test =
  Preface_qcheck.Functor.Make (Functor) (Requirement)
    (Preface_qcheck.Sample.Pack)
module Applicative_test =
  Preface_qcheck.Applicative.Make (Applicative) (Requirement)
    (Preface_qcheck.Sample.Pack)
module Monad_test =
  Preface_qcheck.Monad.Make (Monad) (Requirement) (Preface_qcheck.Sample.Pack)

let cases = Functor_test.cases @ Applicative_test.cases @ Monad_test.cases
