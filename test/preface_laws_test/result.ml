module Bifunctor_suite =
  Preface.Qcheck.Bifunctor.Suite (Req.Result) (Preface.Result.Bifunctor)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)
    (Sample.Int)
    (Sample.String)

module Int_result = Preface.Result.Mono (Sample.Int)

module Invariant_mono_suite =
  Preface.Qcheck.Invariant.Suite
    (Req.Result.Mono (Sample.Int)) (Int_result.Invariant)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Functor_suite =
  Preface.Qcheck.Indexed_functor.Suite (Req.Result) (Preface.Result.Functor)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.String)

module Functor_mono_suite =
  Preface.Qcheck.Functor.Suite
    (Req.Result.Mono (Sample.Int)) (Int_result.Functor)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Alt_suite =
  Preface.Qcheck.Indexed_alt.Suite (Req.Result) (Preface.Result.Alt)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.String)

module Alt_mono_suite =
  Preface.Qcheck.Alt.Suite
    (Req.Result.Mono (Sample.Int)) (Int_result.Alt)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Apply_suite =
  Preface.Qcheck.Indexed_apply.Suite (Req.Result) (Preface.Result.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.String)

module Apply_mono_suite =
  Preface.Qcheck.Apply.Suite
    (Req.Result.Mono (Sample.Int)) (Int_result.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Applicative_suite =
  Preface.Qcheck.Indexed_applicative.Suite
    (Req.Result)
    (Preface.Result.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.String)

module Applicative_mono_suite =
  Preface.Qcheck.Applicative.Suite
    (Req.Result.Mono (Sample.Int)) (Int_result.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Selective_suite =
  Preface.Qcheck.Indexed_selective.Suite (Req.Result) (Preface.Result.Selective)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.String)

module Selective_mono_suite =
  Preface.Qcheck.Selective.Suite_rigid
    (Req.Result.Mono (Sample.Int)) (Int_result.Selective)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Bind_suite =
  Preface.Qcheck.Indexed_bind.Suite (Req.Result) (Preface.Result.Monad)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Int)
    (Sample.String)

module Bind_mono_suite =
  Preface.Qcheck.Bind.Suite
    (Req.Result.Mono (Sample.Int)) (Int_result.Monad)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_suite =
  Preface.Qcheck.Indexed_monad.Suite (Req.Result) (Preface.Result.Monad)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Int)
    (Sample.String)

module Monad_mono_suite =
  Preface.Qcheck.Monad.Suite
    (Req.Result.Mono (Sample.Int)) (Int_result.Monad)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Foldable_suite =
  Preface.Qcheck.Indexed_foldable.Suite (Req.Result) (Preface.Result.Foldable)
    (Sample.Int)
    (Sample.Float)
    (Misc.Sum)
    (Sample.String)

module Foldable_mono_suite =
  Preface.Qcheck.Foldable.Suite
    (Req.Result.Mono (Sample.Int)) (Int_result.Foldable)
    (Sample.Int)
    (Sample.String)
    (Misc.Sum)

module Traversable_monad_mono_suite =
  Preface.Qcheck.Traversable.Suite_monad
    (Req.Result.Mono (Sample.Int)) (Int_result.Monad)
    (Sample.Int)

module Traversable_applicative_mono_suite =
  Preface.Qcheck.Traversable.Suite_applicative
    (Req.Result.Mono (Sample.Int)) (Int_result.Applicative)
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
    ; ("Result Invariant (biased on int)", Invariant_mono_suite.tests)
    ; ("Result Functor (biased on int using Mono)", Functor_mono_suite.tests)
    ; ("Result Functor (biased on int)", Functor_suite.tests)
    ; ("Result Alt (biased on int using Mono)", Alt_mono_suite.tests)
    ; ("Result Alt (biased on int)", Alt_suite.tests)
    ; ("Result Apply (biased on int using Mono)", Apply_mono_suite.tests)
    ; ("Result Apply (biased on int)", Apply_suite.tests)
    ; ( "Result Applicative (biased on int using Mono)"
      , Applicative_mono_suite.tests )
    ; ("Result Applicative (biased on int)", Applicative_suite.tests)
    ; ("Result Selective (biased on int using Mono)", Selective_mono_suite.tests)
    ; ("Result Selective (biased on int)", Selective_suite.tests)
    ; ("Result Bind (biased on int using Mono)", Bind_mono_suite.tests)
    ; ("Result Bind (biased on int)", Bind_suite.tests)
    ; ("Result Monad (biased on int using Mono)", Monad_mono_suite.tests)
    ; ("Result Monad (biased on int)", Monad_suite.tests)
    ; ("Result Foldable (biased on int using Mono)", Foldable_mono_suite.tests)
    ; ("Result Foldable (biased on int)", Foldable_suite.tests)
    ; ( "Result Traversable Monad (biased on int using Mono)"
      , Traversable_monad_mono_suite.tests )
    ; ( "Result Traversable Applicative (biased on int using Mono, using \
         Option and Result)"
      , Traversable_applicative_mono_suite.tests )
    ]
;;
