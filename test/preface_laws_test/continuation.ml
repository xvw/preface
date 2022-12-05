module Invariant_suite =
  Preface.Qcheck.Invariant.Suite
    (Req.Continuation)
    (Preface.Continuation.Invariant)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Functor_suite =
  Preface.Qcheck.Functor.Suite (Req.Continuation) (Preface.Continuation.Functor)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Apply_suite =
  Preface.Qcheck.Apply.Suite
    (Req.Continuation)
    (Preface.Continuation.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Applicative_suite =
  Preface.Qcheck.Applicative.Suite
    (Req.Continuation)
    (Preface.Continuation.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Bind_suite =
  Preface.Qcheck.Bind.Suite (Req.Continuation) (Preface.Continuation.Monad)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_suite =
  Preface.Qcheck.Monad.Suite (Req.Continuation) (Preface.Continuation.Monad)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

let cases ~count =
  Util.with_alcotest ~count
    [
      ("Continuation Invariant", Invariant_suite.tests)
    ; ("Continuation Functor", Functor_suite.tests)
    ; ("Continuation Apply", Apply_suite.tests)
    ; ("Continuation Applicative", Applicative_suite.tests)
    ; ("Continuation Bind", Bind_suite.tests)
    ; ("Continuation Monad", Monad_suite.tests)
    ]
;;
