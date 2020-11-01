open Preface_stdlib.State.Over (struct
  type t = int
end)

module Requirement = struct
  type nonrec 'a t = 'a t

  let name = "State over Int"

  let size = 100

  let arbitrary x = Preface_qcheck.Arbitrary.state x
end

module Hook = struct
  type nonrec 'a t = 'a t

  let apply x =
    Obj.magic
      (let open Monad in
      let v =
        let* _ = x in
        get
      in
      fst (v 42))
  ;;
end

module Functor_test =
  Preface_qcheck.Functor.Make_hooked (Functor) (Requirement) (Hook)
    (Preface_qcheck.Sample.Pack)
module Applicative_test =
  Preface_qcheck.Applicative.Make_hooked (Applicative) (Requirement) (Hook)
    (Preface_qcheck.Sample.Pack)
module Monad_test =
  Preface_qcheck.Monad.Make_hooked (Monad) (Requirement) (Hook)
    (Preface_qcheck.Sample.Pack)

let cases = Functor_test.cases @ Applicative_test.cases @ Monad_test.cases