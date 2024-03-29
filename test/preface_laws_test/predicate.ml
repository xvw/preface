module Invariant_suite =
  Preface.Qcheck.Invariant.Suite_contravariant
    (Req.Predicate)
    (Preface.Predicate.Invariant)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Contravariant_suite =
  Preface.Qcheck.Contravariant.Suite
    (Req.Predicate)
    (Preface.Predicate.Contravariant)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Divisible_suite =
  Preface.Qcheck.Divisible.Suite (Req.Predicate) (Preface.Predicate.Divisible)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Decidable_suite =
  Preface.Qcheck.Decidable.Suite (Req.Predicate) (Preface.Predicate.Decidable)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

let cases ~count =
  Util.with_alcotest ~count
    [
      ("Predicate Invariant", Invariant_suite.tests)
    ; ("Predicate Contravariant", Contravariant_suite.tests)
    ; ("Predicate Divisible", Divisible_suite.tests)
    ; ("Predicate Decidable", Decidable_suite.tests)
    ]
;;
