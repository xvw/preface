module Invariant_suite =
  Preface.Qcheck.Invariant.Suite (Req.Try) (Preface.Try.Invariant) (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Functor_suite =
  Preface.Qcheck.Functor.Suite (Req.Try) (Preface.Try.Functor) (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Alt_suite =
  Preface.Qcheck.Alt.Suite (Req.Try) (Preface.Try.Alt) (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Apply_suite =
  Preface.Qcheck.Apply.Suite (Req.Try) (Preface.Try.Applicative) (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Applicative_suite =
  Preface.Qcheck.Applicative.Suite (Req.Try) (Preface.Try.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Selective_suite =
  Preface.Qcheck.Selective.Suite_rigid (Req.Try) (Preface.Try.Selective)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Bind_suite =
  Preface.Qcheck.Bind.Suite (Req.Try) (Preface.Try.Monad) (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_suite =
  Preface.Qcheck.Monad.Suite (Req.Try) (Preface.Try.Monad) (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Foldable_suite =
  Preface.Qcheck.Foldable.Suite (Req.Try) (Preface.Try.Foldable) (Sample.Int)
    (Sample.String)
    (Misc.Sum)

module Traversable_monad_suite =
  Preface.Qcheck.Traversable.Suite_monad (Req.Try) (Preface.Try.Monad)
    (Sample.Int)

module Traversable_applicative_suite =
  Preface.Qcheck.Traversable.Suite_applicative
    (Req.Try)
    (Preface.Try.Applicative)
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
      ("Try Invariant", Invariant_suite.tests)
    ; ("Try Functor", Functor_suite.tests)
    ; ("Try Alt", Functor_suite.tests)
    ; ("Try Apply", Apply_suite.tests)
    ; ("Try Applicative", Applicative_suite.tests)
    ; ("Try Selective", Selective_suite.tests)
    ; ("Try Bind", Bind_suite.tests)
    ; ("Try Monad", Monad_suite.tests)
    ; ("Try Foldable", Foldable_suite.tests)
    ; ("Try Traversable Monad", Traversable_monad_suite.tests)
    ; ( "Try Traversable Applicative (using Option and Result)"
      , Traversable_applicative_suite.tests )
    ]
;;
