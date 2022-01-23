module Req = struct
  type ('a, 'b) t = ('a, 'b) Preface_stdlib.Result.t

  let arbitrary x y = Preface_qcheck.Arbitrary.result x y
  let observable x y = Preface_qcheck.Observable.result x y
  let equal x y = Preface_stdlib.Result.equal x y
end

module Req_with_int = struct
  type 'a t = ('a, int) Preface_stdlib.Result.t

  let arbitrary x = Preface_qcheck.Arbitrary.result x QCheck.int
  let observable x = Preface_qcheck.Observable.result x QCheck.Observable.int
  let equal f = Preface_stdlib.Result.equal f Int.equal
end

module Functor =
  Preface_laws.Functor.Cases
    (Preface_stdlib.Result.Functor (Int)) (Req_with_int)
    (Preface_qcheck.Sample.Pack1)

module Alt =
  Preface_laws.Alt.Semigroup_cases
    (Preface_stdlib.Result.Alt (Int)) (Req_with_int)
    (Preface_qcheck.Sample.Pack1)

module Applicative =
  Preface_laws.Applicative.Cases
    (Preface_stdlib.Result.Applicative (Int)) (Req_with_int)
    (Preface_qcheck.Sample.Pack1)

module Monad =
  Preface_laws.Monad.Cases
    (Preface_stdlib.Result.Monad (Int)) (Req_with_int)
    (Preface_qcheck.Sample.Pack1)

module Bifunctor =
  Preface_laws.Bifunctor.Cases (Preface_stdlib.Result.Bifunctor) (Req)
    (Preface_qcheck.Sample.Pack1)

let cases n =
  [
    ("Result (with int as Error part) Functor Laws", Functor.cases n)
  ; ("Result (with int as Error part) Alt semigroup Laws", Functor.cases n)
  ; ("Result (with int as Error part) Applicative Laws", Applicative.cases n)
  ; ("Result (with int as Error part) Monad Laws", Monad.cases n)
  ; ("Result Bifunctor Laws", Bifunctor.cases n)
  ]
;;
