module Req = struct
  type ('a, 'b) t = ('a, 'b) Preface_stdlib.Either.t

  let arbitrary x y = Preface_qcheck.Arbitrary.either x y
  let observable x y = Preface_qcheck.Observable.either x y
  let equal left right = Preface_stdlib.Either.equal left right
end

module Req_with_int = struct
  type 'a t = (int, 'a) Preface_stdlib.Either.t

  let arbitrary x = Preface_qcheck.Arbitrary.either QCheck.int x
  let observable x = Preface_qcheck.Observable.either QCheck.Observable.int x
  let equal f = Preface_stdlib.Either.equal Int.equal f
end

module Functor =
  Preface_laws_pbt.Functor.Cases
    (Preface_stdlib.Either.Functor (Int)) (Req_with_int)
    (Preface_qcheck.Sample.Pack1)

module Alt =
  Preface_laws.Alt.Semigroup_cases
    (Preface_stdlib.Either.Alt (Int)) (Req_with_int)
    (Preface_qcheck.Sample.Pack1)

module Applicative =
  Preface_laws_pbt.Applicative.Cases
    (Preface_stdlib.Either.Applicative (Int)) (Req_with_int)
    (Preface_qcheck.Sample.Pack1)

module Selective =
  Preface_laws.Selective.Cases
    (Preface_stdlib.Either.Selective (Int)) (Req_with_int)
    (Preface_qcheck.Sample.Pack1)

module Monad =
  Preface_laws_pbt.Monad.Cases
    (Preface_stdlib.Either.Monad (Int)) (Req_with_int)
    (Preface_qcheck.Sample.Pack1)

module Bifunctor =
  Preface_laws_pbt.Bifunctor.Cases (Preface_stdlib.Either.Bifunctor) (Req)
    (Preface_qcheck.Sample.Pack1)

let cases n =
  [
    ("Either (with int as Left part) Functor Laws", Functor.cases n)
  ; ("Either (with int as Left part) Alt Semigroup Laws", Alt.cases n)
  ; ("Either (with int as Left part) Applicative Laws", Applicative.cases n)
  ; ("Either (with int as Left part) Selective Laws", Selective.cases n)
  ; ("Either (with int as left part) Monad Laws", Monad.cases n)
  ; ("Either Bifunctor Laws", Bifunctor.cases n)
  ]
;;
