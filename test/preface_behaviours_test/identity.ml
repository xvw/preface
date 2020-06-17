open Preface_stdlib.Identity

module Requirement = struct
  type nonrec 'a t = 'a t

  let name = "Identity"

  let size = 100

  let arbitrary x = Preface_qcheck.Arbitrary.identity ?collect:None x
end

module Functor_test =
  Preface_qcheck.Functor.Make (Functor) (Requirement)
    (Preface_qcheck.Sample.Pack)
module Applicative_test =
  Preface_qcheck.Applicative.Make (Applicative) (Requirement)
    (Preface_qcheck.Sample.Pack)
module Monad_test =
  Preface_qcheck.Monad.Make (Monad) (Requirement) (Preface_qcheck.Sample.Pack)
module Selective_test =
  Preface_qcheck.Selective.Make_rigid (Selective) (Requirement)
    (Preface_qcheck.Sample.Pack)

let cases =
  Functor_test.cases
  @ Applicative_test.cases
  @ Monad_test.cases
  @ Selective_test.cases
;;
