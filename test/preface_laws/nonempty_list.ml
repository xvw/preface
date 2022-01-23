module Req = struct
  type 'a t = 'a Preface_stdlib.Nonempty_list.t

  let arbitrary x = Preface_qcheck.Arbitrary.nonempty_list x
  let observable x = Preface_qcheck.Observable.nonempty_list x
  let equal = Preface_stdlib.Nonempty_list.equal
end

module Functor =
  Preface_laws.Functor.Cases (Preface_stdlib.Nonempty_list.Functor) (Req)
    (Preface_qcheck.Sample.Pack1)

module Applicative =
  Preface_laws.Applicative.Cases
    (Preface_stdlib.Nonempty_list.Applicative)
    (Req)
    (Preface_qcheck.Sample.Pack1)

module Selective =
  Preface_laws.Selective.Rigid_cases
    (Preface_stdlib.Nonempty_list.Selective)
    (Req)
    (Preface_qcheck.Sample.Pack1)

module Monad =
  Preface_laws.Monad.Cases (Preface_stdlib.Nonempty_list.Monad) (Req)
    (Preface_qcheck.Sample.Pack1)

module Alt =
  Preface_laws.Alt.Semigroup_cases (Preface_stdlib.Nonempty_list.Alt) (Req)
    (Preface_qcheck.Sample.Pack1)

module Comonad =
  Preface_laws.Comonad.Cases (Preface_stdlib.Nonempty_list.Comonad) (Req)
    (Preface_qcheck.Sample.Pack1)

let cases n =
  [
    ("Nonempty_list Functor Laws", Functor.cases n)
  ; ("Nonempty_list Applicative Laws", Applicative.cases n)
  ; ("Nonempty_list Selective Laws", Selective.cases n)
  ; ("Nonempty_list Monad Laws", Monad.cases n)
  ; ("Nonempty_list Alt Laws", Alt.cases n)
  ; ("Nonempty_list Comonad Laws", Comonad.cases n)
  ]
;;
