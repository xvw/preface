module Invariant_suite =
  Preface.Qcheck.Invariant.Suite (Req.Validate) (Preface.Validate.Invariant)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Functor_suite =
  Preface.Qcheck.Functor.Suite (Req.Validate) (Preface.Validate.Functor)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Apply_suite =
  Preface.Qcheck.Apply.Suite (Req.Validate) (Preface.Validate.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Applicative_suite =
  Preface.Qcheck.Applicative.Suite (Req.Validate) (Preface.Validate.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Selective_suite =
  Preface.Qcheck.Selective.Suite (Req.Validate) (Preface.Validate.Selective)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Bind_suite =
  Preface.Qcheck.Bind.Suite (Req.Validate) (Preface.Validate.Monad) (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_suite =
  Preface.Qcheck.Monad.Suite (Req.Validate) (Preface.Validate.Monad)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Foldable_suite =
  Preface.Qcheck.Foldable.Suite (Req.Validate) (Preface.Validate.Foldable)
    (Sample.Int)
    (Sample.String)
    (Misc.Sum)

module Traversable_monad_suite =
  Preface.Qcheck.Traversable.Suite_monad (Req.Validate) (Preface.Validate.Monad)
    (Sample.Int)

module Traversable_applicative_suite =
  Preface.Qcheck.Traversable.Suite_applicative
    (Req.Validate)
    (Preface.Validate.Applicative)
    (Req.Option)
    (Preface.Option.Applicative)
    (Req.Try)
    (Preface.Try.Applicative)
    (struct
      let run = function None -> Error Not_found | Some x -> Ok x
    end)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

let cases ~count =
  Util.with_alcotest ~count
    [
      ("Validate Invariant", Invariant_suite.tests)
    ; ("Validate Functor", Functor_suite.tests)
    ; ("Validate Apply", Apply_suite.tests)
    ; ("Validate Applicative", Applicative_suite.tests)
    ; ("Validate Selective", Selective_suite.tests)
    ; ("Validate Bind", Bind_suite.tests)
    ; ("Validate Monad", Monad_suite.tests)
    ; ("Validate Foldable", Foldable_suite.tests)
    ; ("Validate Traversable Monad", Traversable_monad_suite.tests)
    ; ( "Validate Traversable Applicative (using Option and Result)"
      , Traversable_applicative_suite.tests )
    ]
;;
