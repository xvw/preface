module Req = struct
  type 'a t = 'a Preface_stdlib.Try.t

  let arbitrary x = Preface_qcheck.Arbitrary.try_ x
  let observable x = Preface_qcheck.Observable.try_ x
  let equal x = Preface_stdlib.Try.equal x
end

module Functor =
  Preface_laws_pbt.Functor.Cases (Preface_stdlib.Try.Functor) (Req)
    (Preface_qcheck.Sample.Pack1)

module Alt =
  Preface_laws_pbt.Alt.Semigroup_cases (Preface_stdlib.Try.Alt) (Req)
    (Preface_qcheck.Sample.Pack1)

module Applicative =
  Preface_laws_pbt.Applicative.Cases (Preface_stdlib.Try.Applicative) (Req)
    (Preface_qcheck.Sample.Pack1)

module Selective =
  Preface_laws_pbt.Selective.Cases (Preface_stdlib.Try.Selective) (Req)
    (Preface_qcheck.Sample.Pack1)

module Monad =
  Preface_laws_pbt.Monad.Cases (Preface_stdlib.Try.Monad) (Req)
    (Preface_qcheck.Sample.Pack1)

let cases n =
  [
    ("Try Functor Laws", Functor.cases n)
  ; ("Try Alt semigroup Laws", Alt.cases n)
  ; ("Try Applicative Laws", Applicative.cases n)
  ; ("Try Selective Laws", Selective.cases n)
  ; ("Try Monad Laws", Monad.cases n)
  ]
;;
