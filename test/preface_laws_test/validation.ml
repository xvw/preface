module Bifunctor_suite =
  Preface.Qcheck.Bifunctor.Suite (Req.Validation) (Preface.Validation.Bifunctor)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)
    (Sample.Int)
    (Sample.String)

module Invariant_suite =
  Preface.Qcheck.Invariant.Suite
    (Req.Validation.Mono
       (Sample.Int))
       (Preface.Validation.Invariant (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Functor_suite =
  Preface.Qcheck.Functor.Suite
    (Req.Validation.Mono (Sample.Int)) (Preface.Validation.Functor (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Alt_suite =
  Preface.Qcheck.Alt.Suite
    (Req.Validation.Mono (Sample.Int)) (Preface.Validation.Alt (Misc.Sum))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Apply_suite =
  Preface.Qcheck.Apply.Suite
    (Req.Validation.Mono
       (Sample.Int))
       (Preface.Validation.Applicative (Misc.Sum))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Applicative_suite =
  Preface.Qcheck.Applicative.Suite
    (Req.Validation.Mono
       (Sample.Int))
       (Preface.Validation.Applicative (Misc.Sum))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Selective_suite =
  Preface.Qcheck.Selective.Suite
    (Req.Validation.Mono (Sample.Int)) (Preface.Validation.Selective (Misc.Sum))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Bind_suite =
  Preface.Qcheck.Bind.Suite
    (Req.Validation.Mono (Sample.Int)) (Preface.Validation.Monad (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_suite =
  Preface.Qcheck.Monad.Suite
    (Req.Validation.Mono (Sample.Int)) (Preface.Validation.Monad (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Foldable_suite =
  Preface.Qcheck.Foldable.Suite
    (Req.Validation.Mono
       (Sample.Int))
       (Preface.Validation.Foldable (Sample.Int))
    (Sample.Int)
    (Sample.String)
    (Misc.Sum)

module Traversable_monad_suite =
  Preface.Qcheck.Traversable.Suite_monad
    (Req.Validation.Mono (Sample.Int)) (Preface.Validation.Monad (Sample.Int))
    (Sample.Int)

module Traversable_applicative_suite =
  Preface.Qcheck.Traversable.Suite_applicative
    (Req.Validation.Mono
       (Sample.Int))
       (Preface.Validation.Applicative (Misc.Sum))
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
      ("Validation Bifunctor", Bifunctor_suite.tests)
    ; ("Validation Invariant (biased on int)", Invariant_suite.tests)
    ; ("Validation Functor (biased on int)", Functor_suite.tests)
    ; ("Validation Alt (biased on int)", Alt_suite.tests)
    ; ("Validation Apply (biased on int)", Apply_suite.tests)
    ; ("Validation Applicative (biased on int)", Applicative_suite.tests)
    ; ("Validation Selective (biased on int)", Selective_suite.tests)
    ; ("Validation Bind (biased on int)", Bind_suite.tests)
    ; ("Validation Monad (biased on int)", Monad_suite.tests)
    ; ("Validation Foldable (biased on int)", Foldable_suite.tests)
    ; ( "Validation Traversable Monad (biased on int)"
      , Traversable_monad_suite.tests )
    ; ( "Validation Traversable Applicative (biased on int, using Option and \
         Result)"
      , Traversable_applicative_suite.tests )
    ]
;;
