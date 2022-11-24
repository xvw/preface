module Contravariant_suite =
  Preface.Qcheck.Contravariant.Suite
    (Req.Equivalence)
    (Preface.Equivalence.Contravariant)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Divisible_suite =
  Preface.Qcheck.Divisible.Suite
    (Req.Equivalence)
    (Preface.Equivalence.Divisible)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Decidable_suite =
  Preface.Qcheck.Decidable.Suite
    (Req.Equivalence)
    (Preface.Equivalence.Decidable)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

let cases ~count =
  Util.with_alcotest ~count
    [
      ("Equivalence Contravariant", Contravariant_suite.tests)
    ; ("Equivalence Divisible", Divisible_suite.tests)
    ; ("Equivalence Decidable", Decidable_suite.tests)
    ]
;;
