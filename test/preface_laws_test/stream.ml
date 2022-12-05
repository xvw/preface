module Invariant_suite =
  Preface.Qcheck.Invariant.Suite (Req.Stream) (Preface.Stream.Invariant)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Functor_suite =
  Preface.Qcheck.Functor.Suite (Req.Stream) (Preface.Stream.Functor)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Apply_suite =
  Preface.Qcheck.Apply.Suite (Req.Stream) (Preface.Stream.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Applicative_suite =
  Preface.Qcheck.Applicative.Suite (Req.Stream) (Preface.Stream.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Bind_suite =
  Preface.Qcheck.Bind.Suite (Req.Stream) (Preface.Stream.Monad) (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_suite =
  Preface.Qcheck.Monad.Suite (Req.Stream) (Preface.Stream.Monad) (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Comonad_suite =
  Preface.Qcheck.Comonad.Suite (Req.Stream) (Preface.Stream.Comonad)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

let cases ~count =
  Util.with_alcotest ~count
    [
      ("Stream Invariant", Invariant_suite.tests)
    ; ("Stream Functor", Functor_suite.tests)
    ; ("Stream Apply", Apply_suite.tests)
    ; ("Stream Applicative", Applicative_suite.tests)
    ; ("Stream Bind", Bind_suite.tests)
    ; ("Stream Monad", Monad_suite.tests)
    ; ("Stream Comonad", Comonad_suite.tests)
    ]
;;
