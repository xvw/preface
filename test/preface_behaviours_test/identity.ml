open Preface_stdlib.Identity

module Requirement = struct
  type nonrec 'a t = 'a t

  let name = "Identity"

  let size = 100

  let arbitrary x = Preface_qcheck.Arbitrary.identity ?collect:None x
end

module Hook = struct
  type nonrec 'a t = 'a t

  let apply x = Obj.magic x
end

module Obs = struct
  type nonrec 'a t = 'a t

  let f obs =
    let print x =
      let p = QCheck.Observable.print obs in
      let value = extract x in
      p value
    in
    QCheck.Observable.make print
  ;;
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
module Comonad_test =
  Preface_qcheck.Comonad.Make_hooked (Comonad) (Requirement) (Hook) (Obs)
    (Preface_qcheck.Sample.Pack)

let cases =
  Functor_test.cases
  @ Applicative_test.cases
  @ Monad_test.cases
  @ Selective_test.cases
  @ Comonad_test.cases
;;
