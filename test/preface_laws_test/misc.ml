module Sum = Preface.Make.Monoid.Via_combine_and_neutral (struct
  type t = int

  let neutral = 0
  let combine = Int.add
end)

module Prod = Preface.Make.Monoid.Via_combine_and_neutral (struct
  type t = int

  let neutral = 1
  let combine = Int.mul
end)

module Sum_monoid_suite = Preface.Qcheck.Monoid.Suite (Sample.Int) (Sum)
module Prod_monoid_suite = Preface.Qcheck.Monoid.Suite (Sample.Int) (Prod)

module YOCaml_profunctor =
  Preface.Qcheck.Profunctor.Suite
    (Req.Mini_yocaml.Req)
    (Req.Mini_yocaml.Profunctor)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)
    (Sample.Bool)
    (Sample.Float)

module YOCaml_strong =
  Preface.Qcheck.Strong.Suite (Req.Mini_yocaml.Req) (Req.Mini_yocaml.Strong)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)
    (Sample.Bool)
    (Sample.Float)

module YOCaml_choice =
  Preface.Qcheck.Choice.Suite (Req.Mini_yocaml.Req) (Req.Mini_yocaml.Choice)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)
    (Sample.Bool)
    (Sample.Float)

module YOCaml_semigroupoid =
  Preface.Qcheck.Semigroupoid.Suite
    (Req.Mini_yocaml.Req)
    (Req.Mini_yocaml.Category)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)

module YOCaml_category =
  Preface.Qcheck.Category.Suite (Req.Mini_yocaml.Req) (Req.Mini_yocaml.Category)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)

module YOCaml_arrow =
  Preface.Qcheck.Arrow.Suite (Req.Mini_yocaml.Req) (Req.Mini_yocaml.Arrow)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)

module YOCaml_arrow_choice =
  Preface.Qcheck.Arrow_choice.Suite
    (Req.Mini_yocaml.Req)
    (Req.Mini_yocaml.Arrow_choice)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)

let cases ~count =
  Util.with_alcotest ~count
    [
      ("Sum monoid", Sum_monoid_suite.tests)
    ; ("Prod Monoid", Sum_monoid_suite.tests)
    ; ("YOCaml Profunctor", YOCaml_profunctor.tests)
    ; ("YOCaml Strong", YOCaml_strong.tests)
    ; ("YOCaml Choice", YOCaml_choice.tests)
    ; ("YOCaml Semigroupoid", YOCaml_semigroupoid.tests)
    ; ("YOCaml Categrory", YOCaml_category.tests)
    ; ("YOCaml Arrow", YOCaml_arrow.tests)
    ; ("YOCaml Arrow Choice", YOCaml_arrow_choice.tests)
    ]
;;
