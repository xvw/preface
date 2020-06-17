open Preface_stdlib.Nonempty_list

module Requirement = struct
  type nonrec 'a t = 'a t

  let name = "Nonempty list"

  let size = 100

  let arbitrary x = Preface_qcheck.Arbitrary.nonempty_list x
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

module Semigroup_test =
  Preface_qcheck.Semigroup.Make
    (Semigroup (struct
      type t = int
    end))
    (struct
      type nonrec t = int t

      let name = "Nonempty list over int"

      let size = 100

      let arbitrary = Preface_qcheck.Arbitrary.(nonempty_list int)
    end)

let cases =
  Functor_test.cases
  @ Applicative_test.cases
  @ Monad_test.cases
  @ Selective_test.cases
  @ Semigroup_test.cases
;;
