module Profunctor_suite =
  Preface.Qcheck.Profunctor.Suite (Req.Fun) (Preface.Fun.Profunctor)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)
    (Sample.Bool)
    (Sample.Float)

module Strong_suite =
  Preface.Qcheck.Strong.Suite (Req.Fun) (Preface.Fun.Strong) (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)
    (Sample.Bool)
    (Sample.Float)

module Choice_suite =
  Preface.Qcheck.Choice.Suite (Req.Fun) (Preface.Fun.Choice) (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)
    (Sample.Bool)
    (Sample.Float)

module Closed_suite =
  Preface.Qcheck.Closed.Suite (Req.Fun) (Preface.Fun.Closed) (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)
    (Sample.Bool)
    (Sample.Float)

module Semigroupoid_suite =
  Preface.Qcheck.Semigroupoid.Suite (Req.Fun) (Preface.Fun.Semigroupoid)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)

module Category_suite =
  Preface.Qcheck.Category.Suite (Req.Fun) (Preface.Fun.Category) (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)

module Arrow_suite =
  Preface.Qcheck.Arrow.Suite (Req.Fun) (Preface.Fun.Arrow) (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)

module Arrow_choice =
  Preface.Qcheck.Arrow_choice.Suite (Req.Fun) (Preface.Fun.Arrow_choice)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)

module Arrow_apply =
  Preface.Qcheck.Arrow_apply.Suite (Req.Fun) (Preface.Fun.Arrow_apply)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)

let cases ~count =
  Util.with_alcotest ~count
    [
      ("Fun Profunctor", Profunctor_suite.tests)
    ; ("Fun Strong", Strong_suite.tests)
    ; ("Fun Choice", Choice_suite.tests)
    ; ("Fun Closed", Closed_suite.tests)
    ; ("Fun Semigroupoid", Semigroupoid_suite.tests)
    ; ("Fun Category", Category_suite.tests)
    ; ("Fun Arrow", Arrow_suite.tests)
    ; ("Fun Arrow Choice", Arrow_choice.tests)
    ; ("Fun Arrow Apply", Arrow_apply.tests)
    ]
;;
