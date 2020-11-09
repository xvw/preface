open Preface_stdlib.Validation

module Requirement = struct
  type nonrec 'a t = ('a, exn list) t

  let name = "Validation"

  let size = 1000

  let arbitrary x = Preface_qcheck.Arbitrary.(validation x (small_list (exn ())))
end

module Alt = Preface_stdlib.List.Alternative

module Error = struct
  type t = Preface_stdlib.Exn.t
end

module ErrorT = struct
  type t = Error.t Alt.t
end

module Exn_list = Preface_make.Semigroup.From_alt (Alt) (Error)
module Functor_test =
  Preface_qcheck.Functor.Make
    (Functor (ErrorT)) (Requirement)
    (Preface_qcheck.Sample.Pack)
module Applicative_test =
  Preface_qcheck.Applicative.Make
    (Applicative (Exn_list)) (Requirement)
    (Preface_qcheck.Sample.Pack)
module Monad_test =
  Preface_qcheck.Monad.Make
    (Monad (ErrorT)) (Requirement)
    (Preface_qcheck.Sample.Pack)
module Selective_test =
  Preface_qcheck.Selective.Make
    (Selective (Exn_list)) (Requirement)
    (Preface_qcheck.Sample.Pack)

let cases =
  Functor_test.cases
  @ Applicative_test.cases
  @ Monad_test.cases
  @ Selective_test.cases
;;
