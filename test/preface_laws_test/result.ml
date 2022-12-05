module Bifunctor_suite =
  Preface.Qcheck.Bifunctor.Suite (Req.Result) (Preface.Result.Bifunctor)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)
    (Sample.Int)
    (Sample.String)

module Invariant_suite =
  Preface.Qcheck.Invariant.Suite
    (Req.Result.Mono (Sample.Int)) (Preface.Result.Invariant (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Functor_suite =
  Preface.Qcheck.Functor.Suite
    (Req.Result.Mono (Sample.Int)) (Preface.Result.Functor (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Alt_suite =
  Preface.Qcheck.Alt.Suite
    (Req.Result.Mono (Sample.Int)) (Preface.Result.Alt (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Apply_suite =
  Preface.Qcheck.Apply.Suite
    (Req.Result.Mono (Sample.Int)) (Preface.Result.Applicative (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Applicative_suite =
  Preface.Qcheck.Applicative.Suite
    (Req.Result.Mono (Sample.Int)) (Preface.Result.Applicative (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Selective_suite =
  Preface.Qcheck.Selective.Suite_rigid
    (Req.Result.Mono (Sample.Int)) (Preface.Result.Selective (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Bind_suite =
  Preface.Qcheck.Bind.Suite
    (Req.Result.Mono (Sample.Int)) (Preface.Result.Monad (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_suite =
  Preface.Qcheck.Monad.Suite
    (Req.Result.Mono (Sample.Int)) (Preface.Result.Monad (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Foldable_suite =
  Preface.Qcheck.Foldable.Suite
    (Req.Result.Mono (Sample.Int)) (Preface.Result.Foldable (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Misc.Sum)

module Traversable_monad_suite =
  Preface.Qcheck.Traversable.Suite_monad
    (Req.Result.Mono (Sample.Int)) (Preface.Result.Monad (Sample.Int))
    (Sample.Int)

module Traversable_applicative_suite =
  Preface.Qcheck.Traversable.Suite_applicative
    (Req.Result.Mono (Sample.Int)) (Preface.Result.Applicative (Sample.Int))
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
      ("Result Bifunctor", Bifunctor_suite.tests)
    ; ("Result Invariant (biased on int)", Invariant_suite.tests)
    ; ("Result Functor (biased on int)", Functor_suite.tests)
    ; ("Result Alt (biased on int)", Alt_suite.tests)
    ; ("Result Apply (biased on int)", Apply_suite.tests)
    ; ("Result Applicative (biased on int)", Applicative_suite.tests)
    ; ("Result Selective (biased on int)", Selective_suite.tests)
    ; ("Result Bind (biased on int)", Bind_suite.tests)
    ; ("Result Monad (biased on int)", Monad_suite.tests)
    ; ("Result Foldable (biased on int)", Foldable_suite.tests)
    ; ("Result Traversable Monad (biased on int)", Traversable_monad_suite.tests)
    ; ( "Result Traversable Applicative (biased on int, using Option and Result)"
      , Traversable_applicative_suite.tests )
    ]
;;
