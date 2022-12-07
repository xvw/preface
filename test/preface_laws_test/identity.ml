module Invariant_suite =
  Preface.Qcheck.Invariant.Suite (Req.Identity) (Preface.Identity.Invariant)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Functor_suite =
  Preface.Qcheck.Functor.Suite (Req.Identity) (Preface.Identity.Functor)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Apply_suite =
  Preface.Qcheck.Apply.Suite (Req.Identity) (Preface.Identity.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Applicative_suite =
  Preface.Qcheck.Applicative.Suite (Req.Identity) (Preface.Identity.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Selective_suite =
  Preface.Qcheck.Selective.Suite_rigid
    (Req.Identity)
    (Preface.Identity.Selective)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Bind_suite =
  Preface.Qcheck.Bind.Suite (Req.Identity) (Preface.Identity.Monad) (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_suite =
  Preface.Qcheck.Monad.Suite (Req.Identity) (Preface.Identity.Monad)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Comonad_suite =
  Preface.Qcheck.Comonad.Suite (Req.Identity) (Preface.Identity.Comonad)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

let cases ~count =
  Util.with_alcotest ~count
    [
      ("Identity Invariant", Invariant_suite.tests)
    ; ("Identity Functor", Functor_suite.tests)
    ; ("Identity Apply", Apply_suite.tests)
    ; ("Identity Applicative", Applicative_suite.tests)
    ; ("Identity Selective", Selective_suite.tests)
    ; ("Identity Bind", Bind_suite.tests)
    ; ("Identity Monad", Monad_suite.tests)
    ; ("Identity Comonad", Comonad_suite.tests)
    ]
;;
