module Bifunctor_suite =
  Preface.Qcheck.Bifunctor.Suite (Req.Either) (Preface.Either.Bifunctor)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)
    (Sample.Int)
    (Sample.String)

module Invariant_suite =
  Preface.Qcheck.Invariant.Suite
    (Req.Either.Mono (Sample.Int)) (Preface.Either.Invariant (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Functor_suite =
  Preface.Qcheck.Functor.Suite
    (Req.Either.Mono (Sample.Int)) (Preface.Either.Functor (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Alt_suite =
  Preface.Qcheck.Alt.Suite
    (Req.Either.Mono (Sample.Int)) (Preface.Either.Alt (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Apply_suite =
  Preface.Qcheck.Apply.Suite
    (Req.Either.Mono (Sample.Int)) (Preface.Either.Applicative (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Applicative_suite =
  Preface.Qcheck.Applicative.Suite
    (Req.Either.Mono (Sample.Int)) (Preface.Either.Applicative (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Selective_suite =
  Preface.Qcheck.Selective.Suite_rigid
    (Req.Either.Mono (Sample.Int)) (Preface.Either.Selective (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Bind_suite =
  Preface.Qcheck.Bind.Suite
    (Req.Either.Mono (Sample.Int)) (Preface.Either.Monad (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_suite =
  Preface.Qcheck.Monad.Suite
    (Req.Either.Mono (Sample.Int)) (Preface.Either.Monad (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Foldable_suite =
  Preface.Qcheck.Foldable.Suite
    (Req.Either.Mono (Sample.Int)) (Preface.Either.Foldable (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Misc.Sum)

module Traversable_monad_suite =
  Preface.Qcheck.Traversable.Suite_monad
    (Req.Either.Mono (Sample.Int)) (Preface.Either.Monad (Sample.Int))
    (Sample.Int)

module Traversable_applicative_suite =
  Preface.Qcheck.Traversable.Suite_applicative
    (Req.Either.Mono (Sample.Int)) (Preface.Either.Applicative (Sample.Int))
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
      ("Either Bifunctor", Bifunctor_suite.tests)
    ; ("Either Invariant (biased on int)", Invariant_suite.tests)
    ; ("Either Functor (biased on int)", Functor_suite.tests)
    ; ("Either Alt (biased on int)", Alt_suite.tests)
    ; ("Either Apply (biased on int)", Apply_suite.tests)
    ; ("Either Applicative (biased on int)", Applicative_suite.tests)
    ; ("Either Selective (biased on int)", Selective_suite.tests)
    ; ("Either Bind (biased on int)", Bind_suite.tests)
    ; ("Either Monad (biased on int)", Monad_suite.tests)
    ; ("Either Foldable (biased on int)", Foldable_suite.tests)
    ; ("Either Traversable Monad (biased on int)", Traversable_monad_suite.tests)
    ; ( "Either Traversable Applicative (biased on int, using Option and Result)"
      , Traversable_applicative_suite.tests )
    ]
;;
