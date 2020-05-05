open Preface_stdlib.Option

module Requirement = struct
  type nonrec 'a t = 'a t

  let name = "Option"

  let size = 100

  let arbitrary x = Preface_qcheck.Arbitrary.option x
end

module Functor_test =
  Preface_qcheck.Functor.Make (Functor) (Requirement)
    (Preface_qcheck.Sample.Pack)
module Applicative_test =
  Preface_qcheck.Applicative.Make (Applicative) (Requirement)
    (Preface_qcheck.Sample.Pack)
module Monad_test =
  Preface_qcheck.Monad.Make (Monad) (Requirement) (Preface_qcheck.Sample.Pack)

module Monoid_test =
  Preface_qcheck.Monoid.Make
    (Monoid
       (Misc.Sum_semigroup))
       (struct
         type nonrec t = int t

         let name = "Option over Sum int"

         let size = 100

         let arbitrary = Preface_qcheck.Arbitrary.(option int)
       end)

let cases =
  Functor_test.cases
  @ Applicative_test.cases
  @ Monad_test.cases
  @ Monoid_test.cases
;;
