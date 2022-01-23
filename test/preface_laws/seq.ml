module Req = struct
  type 'a t = 'a Preface_stdlib.Seq.t

  let arbitrary x = Preface_qcheck.Arbitrary.seq x
  let observable x = Preface_qcheck.Observable.seq x
  let equal x = Preface_stdlib.Seq.equal x
end

module Functor =
  Preface_laws.Functor.Cases (Preface_stdlib.Seq.Functor) (Req)
    (Preface_qcheck.Sample.Pack1)

module Applicative =
  Preface_laws.Applicative.Cases (Preface_stdlib.Seq.Applicative) (Req)
    (Preface_qcheck.Sample.Pack1)

module Selective =
  Preface_laws.Selective.Rigid_cases (Preface_stdlib.Seq.Selective) (Req)
    (Preface_qcheck.Sample.Pack1)

module Alternative =
  Preface_laws.Alternative.Cases (Preface_stdlib.Seq.Alternative) (Req)
    (Preface_qcheck.Sample.Pack1)

module Monad =
  Preface_laws.Monad.Cases (Preface_stdlib.Seq.Monad) (Req)
    (Preface_qcheck.Sample.Pack1)

module Monad_plus_monoidal =
  Preface_laws.Monad_plus.Cases_for_monoidal
    (Preface_stdlib.Seq.Monad_plus)
    (Req)
    (Preface_qcheck.Sample.Pack1)

module Monad_plus_left_distribution =
  Preface_laws.Monad_plus.Cases_for_bind_left_distributivity
    (Preface_stdlib.Seq.Monad_plus)
    (Req)
    (Preface_qcheck.Sample.Pack1)

module Monad_plus_left_absorption =
  Preface_laws.Monad_plus.Cases_for_left_absorption
    (Preface_stdlib.Seq.Monad_plus)
    (Req)
    (Preface_qcheck.Sample.Pack1)

let cases n =
  [
    ("Seq Functor Laws", Functor.cases n)
  ; ("Seq Applicative Laws", Applicative.cases n)
  ; ("Seq Selective Laws", Selective.cases n)
  ; ("Seq Alternative Laws", Alternative.cases n)
  ; ("Seq Monad Laws", Monad.cases n)
  ; ("Seq Monad Plus (monoidal) Laws", Monad_plus_monoidal.cases n)
  ; ( "Seq Monad Plus (left distribution) Laws"
    , Monad_plus_left_distribution.cases n )
  ; ("Seq Monad Plus (left absorption) Laws", Monad_plus_left_absorption.cases n)
  ]
;;
