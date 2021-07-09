module Req = struct
  type 'a t = 'a Preface_stdlib.Identity.t

  let arbitrary x = Preface_qcheck.Arbitrary.identity x
  let observable x = Preface_qcheck.Observable.identity x
  let equal x = Preface_stdlib.Identity.equal x
end

module Functor =
  Preface_laws_pbt.Functor.Cases (Preface_stdlib.Identity.Functor) (Req)
    (Preface_qcheck.Sample.Pack1)

module Applicative =
  Preface_laws_pbt.Applicative.Cases (Preface_stdlib.Identity.Applicative) (Req)
    (Preface_qcheck.Sample.Pack1)

module Selective =
  Preface_laws_pbt.Selective.Rigid_cases
    (Preface_stdlib.Identity.Selective)
    (Req)
    (Preface_qcheck.Sample.Pack1)

module Monad =
  Preface_laws_pbt.Monad.Cases (Preface_stdlib.Identity.Monad) (Req)
    (Preface_qcheck.Sample.Pack1)

module Comonad =
  Preface_laws_pbt.Comonad.Cases (Preface_stdlib.Identity.Comonad) (Req)
    (Preface_qcheck.Sample.Pack1)

let cases n =
  [
    ("Identity Functor Laws", Functor.cases n)
  ; ("Identity Applicative Laws", Applicative.cases n)
  ; ("Identity Selective Laws", Selective.cases n)
  ; ("Identity Monad Laws", Monad.cases n)
  ; ("Identity Comonad Laws", Comonad.cases n)
  ]
;;
