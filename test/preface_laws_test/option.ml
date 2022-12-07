module Invariant_suite =
  Preface.Qcheck.Invariant.Suite (Req.Option) (Preface.Option.Invariant)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Functor_suite =
  Preface.Qcheck.Functor.Suite (Req.Option) (Preface.Option.Functor)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Alt_suite =
  Preface.Qcheck.Alt.Suite (Req.Option) (Preface.Option.Alternative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Apply_suite =
  Preface.Qcheck.Apply.Suite (Req.Option) (Preface.Option.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Applicative_suite =
  Preface.Qcheck.Applicative.Suite (Req.Option) (Preface.Option.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Alternative_suite =
  Preface.Qcheck.Alternative.Suite (Req.Option) (Preface.Option.Alternative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Selective_suite =
  Preface.Qcheck.Selective.Suite_rigid (Req.Option) (Preface.Option.Selective)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Bind_suite =
  Preface.Qcheck.Bind.Suite (Req.Option) (Preface.Option.Monad) (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_suite =
  Preface.Qcheck.Monad.Suite (Req.Option) (Preface.Option.Monad) (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_plus_monoidal_suite =
  Preface.Qcheck.Monad_plus.Suite_monoidal
    (Req.Option)
    (Preface.Option.Monad_plus)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_plus_absorb_suite =
  Preface.Qcheck.Monad_plus.Suite_left_absorption
    (Req.Option)
    (Preface.Option.Monad_plus)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_plus_catch_suite =
  Preface.Qcheck.Monad_plus.Suite_left_catch
    (Req.Option)
    (Preface.Option.Monad_plus)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Foldable_suite =
  Preface.Qcheck.Foldable.Suite (Req.Option) (Preface.Option.Foldable)
    (Sample.Int)
    (Sample.String)
    (Misc.Sum)

module Traversable_monad_suite =
  Preface.Qcheck.Traversable.Suite_monad (Req.Option) (Preface.Option.Monad)
    (Sample.Int)

module Traversable_applicative_suite =
  Preface.Qcheck.Traversable.Suite_applicative
    (Req.Option)
    (Preface.Option.Applicative)
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
      ("Option Invariant", Invariant_suite.tests)
    ; ("Option Functor", Functor_suite.tests)
    ; ("Option Alt", Alt_suite.tests)
    ; ("Option Apply", Apply_suite.tests)
    ; ("Option Applicative", Applicative_suite.tests)
    ; ("Option Alternative", Alternative_suite.tests)
    ; ("Option Selective", Selective_suite.tests)
    ; ("Option Bind", Bind_suite.tests)
    ; ("Option Monad", Monad_suite.tests)
    ; ("Option Monad Plus (Monoidal)", Monad_plus_monoidal_suite.tests)
    ; ("Option Monad Plus (Left Absorption)", Monad_plus_absorb_suite.tests)
    ; ("Option Monad Plus (Left Catch)", Monad_plus_catch_suite.tests)
    ; ("Option Foldable", Foldable_suite.tests)
    ; ("Option Traversable Monad", Traversable_monad_suite.tests)
    ; ( "Option Traversable Applicative (using Option and Result)"
      , Traversable_applicative_suite.tests )
    ]
;;
