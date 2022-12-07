module Monad_sum_suite =
  Preface.Qcheck.Writer.Suite (Req.Identity) (Preface.Identity.Monad)
    (Sample.Int)
    (Misc.Sum)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)

module Invariant_sum_suite =
  Preface.Qcheck.Writer.Suite_functor (Req.Identity) (Preface.Identity.Functor)
    (Sample.Int)
    (Misc.Sum)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Functor_sum_suite =
  Preface.Qcheck.Writer.Suite_functor (Req.Identity) (Preface.Identity.Functor)
    (Sample.Int)
    (Misc.Sum)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Applicative_sum_suite =
  Preface.Qcheck.Writer.Suite_applicative
    (Req.Identity)
    (Preface.Identity.Applicative)
    (Sample.Int)
    (Misc.Sum)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

let cases ~count =
  Util.with_alcotest ~count
    [
      ("Identity Writer Monad over Sum monoid", Monad_sum_suite.tests)
    ; ("Identity Writer Functor Over Sum monoid", Functor_sum_suite.tests)
    ; ("Identity Writer Applicative Over Sum Monoid", Functor_sum_suite.tests)
    ; ("Identity Writer Invariant Over Sum Monoid", Invariant_sum_suite.tests)
    ]
;;
