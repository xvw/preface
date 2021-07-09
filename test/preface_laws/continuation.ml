module Req = struct
  type 'a t = 'a Preface_stdlib.Continuation.t

  let arbitrary x = Preface_qcheck.Arbitrary.continuation x
  let observable x = Preface_qcheck.Observable.continuation x

  let equal f x y =
    Preface_stdlib.Continuation.(f (x.run Stdlib.Fun.id) (y.run Stdlib.Fun.id))
  ;;
end

module Functor =
  Preface_laws_pbt.Functor.Cases (Preface_stdlib.Continuation.Functor) (Req)
    (Preface_qcheck.Sample.Pack1)

module Applicative =
  Preface_laws_pbt.Applicative.Cases
    (Preface_stdlib.Continuation.Applicative)
    (Req)
    (Preface_qcheck.Sample.Pack1)

module Monad =
  Preface_laws_pbt.Monad.Cases (Preface_stdlib.Continuation.Monad) (Req)
    (Preface_qcheck.Sample.Pack1)

let cases n =
  [
    ("Continuation Functor Laws", Functor.cases n)
  ; ("Continuation Applicative Laws", Applicative.cases n)
  ; ("Continuation Monad Laws", Monad.cases n)
  ]
;;
