module Req = struct
  type 'a t = 'a Preface_stdlib.Validate.t

  let arbitrary x = Preface_qcheck.Arbitrary.validate x
  let observable x = Preface_qcheck.Observable.validate x
  let equal x y = Preface_stdlib.Validate.equal x y
end

module Functor =
  Preface_laws_pbt.Functor.Cases (Preface_stdlib.Validate.Functor) (Req)
    (Preface_qcheck.Sample.Pack1)

module Alt =
  Preface_laws.Alt.Semigroup_cases (Preface_stdlib.Validate.Alt) (Req)
    (Preface_qcheck.Sample.Pack1)

module Applicative =
  Preface_laws_pbt.Applicative.Cases (Preface_stdlib.Validate.Applicative) (Req)
    (Preface_qcheck.Sample.Pack1)

module Monad =
  Preface_laws_pbt.Monad.Cases (Preface_stdlib.Validate.Monad) (Req)
    (Preface_qcheck.Sample.Pack1)

module Selective =
  Preface_laws_pbt.Selective.Cases (Preface_stdlib.Validate.Selective) (Req)
    (Preface_qcheck.Sample.Pack1)

let cases n =
  [
    ("Validate Functor Laws", Functor.cases n)
  ; ("Validate Alt semigroup Laws", Alt.cases n)
  ; ("Validate Applicative Laws", Applicative.cases n)
  ; ("Validate Monad Laws", Monad.cases n)
  ; ("Validate Selective Laws", Selective.cases n)
  ]
;;
