module Req = struct
  type 'a t = 'a Preface_stdlib.Option.t

  let arbitrary x = Preface_qcheck.Arbitrary.option x
  let observable x = Preface_qcheck.Observable.option x
  let equal x = Preface_stdlib.Option.equal x
end

module Functor =
  Preface_laws.Functor.Cases (Preface_stdlib.Option.Functor) (Req)
    (Preface_qcheck.Sample.Pack1)

module Applicative =
  Preface_laws.Applicative.Cases (Preface_stdlib.Option.Applicative) (Req)
    (Preface_qcheck.Sample.Pack1)

module Monad =
  Preface_laws.Monad.Cases (Preface_stdlib.Option.Monad) (Req)
    (Preface_qcheck.Sample.Pack1)

module Alternative =
  Preface_laws.Alternative.Cases (Preface_stdlib.Option.Alternative) (Req)
    (Preface_qcheck.Sample.Pack1)

let cases n =
  [
    ("Option Functor Laws", Functor.cases n)
  ; ("Option Applicative Laws", Applicative.cases n)
  ; ("Option Monad Laws", Monad.cases n)
  ; ("Option Alternative Laws", Alternative.cases n)
  ]
;;
