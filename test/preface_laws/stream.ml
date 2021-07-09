module Req = struct
  type 'a t = 'a Preface_stdlib.Stream.t

  let arbitrary x = Preface_qcheck.Arbitrary.stream x
  let observable x = Preface_qcheck.Observable.stream ~fuel:10 x

  let equal f x y =
    let a = Preface_stdlib.Stream.take 10 x
    and b = Preface_stdlib.Stream.take 10 y in
    Preface_stdlib.(Try.equal (List.equal f)) a b
  ;;
end

module Functor =
  Preface_laws_pbt.Functor.Cases (Preface_stdlib.Stream.Functor) (Req)
    (Preface_qcheck.Sample.Pack1)

module Applicative =
  Preface_laws_pbt.Applicative.Cases (Preface_stdlib.Stream.Applicative) (Req)
    (Preface_qcheck.Sample.Pack1)

module Monad =
  Preface_laws_pbt.Monad.Cases (Preface_stdlib.Stream.Monad) (Req)
    (Preface_qcheck.Sample.Pack1)

module Comonad =
  Preface_laws_pbt.Comonad.Cases (Preface_stdlib.Stream.Comonad) (Req)
    (Preface_qcheck.Sample.Pack1)

let cases n =
  [
    ("Stream Functor Laws", Functor.cases n)
  ; ("Stream Applicative Laws", Applicative.cases n)
  ; ("Stream Monad Laws", Monad.cases n)
  ; ("Stream Comonad Laws", Comonad.cases n)
  ]
;;
