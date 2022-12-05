module Invariant_suite =
  Preface.Qcheck.Invariant.Suite (Req.Seq) (Preface.Seq.Invariant) (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Functor_suite =
  Preface.Qcheck.Functor.Suite (Req.Seq) (Preface.Seq.Functor) (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Alt_suite =
  Preface.Qcheck.Alt.Suite (Req.Seq) (Preface.Seq.Alternative) (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Apply_suite =
  Preface.Qcheck.Apply.Suite (Req.Seq) (Preface.Seq.Applicative) (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Applicative_suite =
  Preface.Qcheck.Applicative.Suite (Req.Seq) (Preface.Seq.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Alternative_suite =
  Preface.Qcheck.Alternative.Suite (Req.Seq) (Preface.Seq.Alternative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Selective_suite =
  Preface.Qcheck.Selective.Suite_rigid (Req.Seq) (Preface.Seq.Selective)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Bind_suite =
  Preface.Qcheck.Bind.Suite (Req.Seq) (Preface.Seq.Monad) (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_suite =
  Preface.Qcheck.Monad.Suite (Req.Seq) (Preface.Seq.Monad) (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_plus_monoidal_suite =
  Preface.Qcheck.Monad_plus.Suite_monoidal (Req.Seq) (Preface.Seq.Monad_plus)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_plus_absorb_suite =
  Preface.Qcheck.Monad_plus.Suite_left_absorption
    (Req.Seq)
    (Preface.Seq.Monad_plus)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_plus_distributivity_suite =
  Preface.Qcheck.Monad_plus.Suite_left_distributivity
    (Req.Seq)
    (Preface.Seq.Monad_plus)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Foldable_suite =
  Preface.Qcheck.Foldable.Suite (Req.Seq) (Preface.Seq.Foldable) (Sample.Int)
    (Sample.String)
    (Misc.Sum)

module Traversable_monad_suite =
  Preface.Qcheck.Traversable.Suite_monad (Req.Seq) (Preface.Seq.Monad)
    (Sample.Int)

module Traversable_applicative_suite =
  Preface.Qcheck.Traversable.Suite_applicative
    (Req.Seq)
    (Preface.Seq.Applicative)
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
      ("Seq Invariant", Invariant_suite.tests)
    ; ("Seq Functor", Functor_suite.tests)
    ; ("Seq Alt", Alt_suite.tests)
    ; ("Seq Apply", Apply_suite.tests)
    ; ("Seq Applicative", Applicative_suite.tests)
    ; ("Seq Alternative", Alternative_suite.tests)
    ; ("Seq Selective", Selective_suite.tests)
    ; ("Seq Bind", Bind_suite.tests)
    ; ("Seq Monad", Monad_suite.tests)
    ; ("Seq Monad Plus (Monoidal)", Monad_plus_monoidal_suite.tests)
    ; ("Seq Monad Plus (Left Absorption)", Monad_plus_absorb_suite.tests)
    ; ( "Seq Monad Plus (Left Distributivity)"
      , Monad_plus_distributivity_suite.tests )
    ; ("Seq Foldable", Foldable_suite.tests)
    ; ("Seq Traversable Monad", Traversable_monad_suite.tests)
    ; ( "Seq Traversable Applicative (using Option and Result)"
      , Traversable_applicative_suite.tests )
    ]
;;
