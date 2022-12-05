module Invariant_suite =
  Preface.Qcheck.Invariant.Suite
    (Req.Nonempty_list)
    (Preface.Nonempty_list.Invariant)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Functor_suite =
  Preface.Qcheck.Functor.Suite
    (Req.Nonempty_list)
    (Preface.Nonempty_list.Functor)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Alt_suite =
  Preface.Qcheck.Alt.Suite (Req.Nonempty_list) (Preface.Nonempty_list.Alt)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Apply_suite =
  Preface.Qcheck.Apply.Suite
    (Req.Nonempty_list)
    (Preface.Nonempty_list.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Applicative_suite =
  Preface.Qcheck.Applicative.Suite
    (Req.Nonempty_list)
    (Preface.Nonempty_list.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Selective_suite =
  Preface.Qcheck.Selective.Suite_rigid
    (Req.Nonempty_list)
    (Preface.Nonempty_list.Selective)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Bind_suite =
  Preface.Qcheck.Bind.Suite (Req.Nonempty_list) (Preface.Nonempty_list.Monad)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Monad_suite =
  Preface.Qcheck.Monad.Suite (Req.Nonempty_list) (Preface.Nonempty_list.Monad)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

module Foldable_suite =
  Preface.Qcheck.Foldable.Suite
    (Req.Nonempty_list)
    (Preface.Nonempty_list.Foldable)
    (Sample.Int)
    (Sample.String)
    (Misc.Sum)

module Traversable_monad_suite =
  Preface.Qcheck.Traversable.Suite_monad
    (Req.Nonempty_list)
    (Preface.Nonempty_list.Monad)
    (Sample.Int)

module Traversable_applicative_suite =
  Preface.Qcheck.Traversable.Suite_applicative
    (Req.Nonempty_list)
    (Preface.Nonempty_list.Applicative)
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

module Semigroup_suite =
  Preface.Qcheck.Semigroup.Suite
    (Req.Nonempty_list.Mono
       (Sample.Int))
       (Preface.Nonempty_list.Semigroup (Sample.Int))

module Comonad_suite =
  Preface.Qcheck.Comonad.Suite
    (Req.Nonempty_list)
    (Preface.Nonempty_list.Comonad)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Bool)

let cases ~count =
  Util.with_alcotest ~count
    [
      ("Nonempty_list Semigroup (biased on Int)", Semigroup_suite.tests)
    ; ("Nonempty_list Invariant", Invariant_suite.tests)
    ; ("Nonempty_list Functor", Functor_suite.tests)
    ; ("Nonempty_list Alt", Alt_suite.tests)
    ; ("Nonempty_list Apply", Apply_suite.tests)
    ; ("Nonempty_list Applicative", Applicative_suite.tests)
    ; ("Nonempty_list Selective", Selective_suite.tests)
    ; ("Nonempty_list Bind", Bind_suite.tests)
    ; ("Nonempty_list Monad", Monad_suite.tests)
    ; ("Nonempty_list Foldable", Foldable_suite.tests)
    ; ("Nonempty_list Traversable Monad", Traversable_monad_suite.tests)
    ; ( "Nonempty_list Traversable Applicative (using Option and Result)"
      , Traversable_applicative_suite.tests )
    ; ("Nonempty_list Comonad", Comonad_suite.tests)
    ]
;;
