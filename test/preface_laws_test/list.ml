module Invariant_suite =
  Preface.Qcheck.Invariant.Suite (Req.List) (Preface.List.Invariant)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Functor_suite =
  Preface.Qcheck.Functor.Suite (Req.List) (Preface.List.Functor) (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Alt_suite =
  Preface.Qcheck.Alt.Suite (Req.List) (Preface.List.Alternative) (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Apply_suite =
  Preface.Qcheck.Apply.Suite (Req.List) (Preface.List.Applicative) (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Applicative_suite =
  Preface.Qcheck.Applicative.Suite (Req.List) (Preface.List.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Alternative_suite =
  Preface.Qcheck.Alternative.Suite (Req.List) (Preface.List.Alternative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Selective_suite =
  Preface.Qcheck.Selective.Suite_rigid (Req.List) (Preface.List.Selective)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Bind_suite =
  Preface.Qcheck.Bind.Suite (Req.List) (Preface.List.Monad) (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_suite =
  Preface.Qcheck.Monad.Suite (Req.List) (Preface.List.Monad) (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_plus_monoidal_suite =
  Preface.Qcheck.Monad_plus.Suite_monoidal (Req.List) (Preface.List.Monad_plus)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_plus_absorb_suite =
  Preface.Qcheck.Monad_plus.Suite_left_absorption
    (Req.List)
    (Preface.List.Monad_plus)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_plus_distributivity_suite =
  Preface.Qcheck.Monad_plus.Suite_left_distributivity
    (Req.List)
    (Preface.List.Monad_plus)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Foldable_suite =
  Preface.Qcheck.Foldable.Suite (Req.List) (Preface.List.Foldable) (Sample.Int)
    (Sample.String)
    (Misc.Sum)

module Traversable_monad_suite =
  Preface.Qcheck.Traversable.Suite_monad (Req.List) (Preface.List.Monad)
    (Sample.Int)

module Traversable_applicative_suite =
  Preface.Qcheck.Traversable.Suite_applicative
    (Req.List)
    (Preface.List.Applicative)
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
      ("List Invariant", Invariant_suite.tests)
    ; ("List Functor", Functor_suite.tests)
    ; ("List Alt", Alt_suite.tests)
    ; ("List Apply", Apply_suite.tests)
    ; ("List Applicative", Applicative_suite.tests)
    ; ("List Alternative", Alternative_suite.tests)
    ; ("List Selective", Selective_suite.tests)
    ; ("List Bind", Bind_suite.tests)
    ; ("List Monad", Monad_suite.tests)
    ; ("List Monad Plus (Monoidal)", Monad_plus_monoidal_suite.tests)
    ; ("List Monad Plus (Left Absorption)", Monad_plus_absorb_suite.tests)
    ; ( "List Monad Plus (Left Distributivity)"
      , Monad_plus_distributivity_suite.tests )
    ; ("List Foldable", Foldable_suite.tests)
    ; ("List Traversable Monad", Traversable_monad_suite.tests)
    ; ( "List Traversable Applicative (using Option and Result)"
      , Traversable_applicative_suite.tests )
    ]
;;
