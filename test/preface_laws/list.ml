module Req = struct
  type 'a t = 'a Preface_stdlib.List.t

  let arbitrary x = Preface_qcheck.Arbitrary.small_list x
  let observable x = Preface_qcheck.Observable.list x
  let equal x = Preface_stdlib.List.equal x
end

module Functor =
  Preface_laws_pbt.Functor.Cases (Preface_stdlib.List.Functor) (Req)
    (Preface_qcheck.Sample.Pack1)

module Applicative =
  Preface_laws_pbt.Applicative.Cases (Preface_stdlib.List.Applicative) (Req)
    (Preface_qcheck.Sample.Pack1)

module Selective =
  Preface_laws_pbt.Selective.Rigid_cases (Preface_stdlib.List.Selective) (Req)
    (Preface_qcheck.Sample.Pack1)

module Alternative =
  Preface_laws_pbt.Alternative.Cases (Preface_stdlib.List.Alternative) (Req)
    (Preface_qcheck.Sample.Pack1)

module Monad =
  Preface_laws_pbt.Monad.Cases (Preface_stdlib.List.Monad) (Req)
    (Preface_qcheck.Sample.Pack1)

module Monad_plus_monoidal =
  Preface_laws_pbt.Monad_plus.Cases_for_monoidal
    (Preface_stdlib.List.Monad_plus)
    (Req)
    (Preface_qcheck.Sample.Pack1)

module Monad_plus_left_distribution =
  Preface_laws_pbt.Monad_plus.Cases_for_bind_left_distributivity
    (Preface_stdlib.List.Monad_plus)
    (Req)
    (Preface_qcheck.Sample.Pack1)

module Monad_plus_left_absorption =
  Preface_laws_pbt.Monad_plus.Cases_for_left_absorption
    (Preface_stdlib.List.Monad_plus)
    (Req)
    (Preface_qcheck.Sample.Pack1)

let cases n =
  [
    ("List Functor Laws", Functor.cases n)
  ; ("List Applicative Laws", Applicative.cases n)
  ; ("List Selective Laws", Selective.cases n)
  ; ("List Alternative Laws", Alternative.cases n)
  ; ("List Monad Laws", Monad.cases n)
  ; ("List Monad Plus (monoidal) Laws", Monad_plus_monoidal.cases n)
  ; ( "List Monad Plus (left distribution) Laws"
    , Monad_plus_left_distribution.cases n )
  ; ( "List Monad Plus (left absorption) Laws"
    , Monad_plus_left_absorption.cases n )
  ]
;;
