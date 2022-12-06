module Over_sum = Req.Over (Misc.Sum) (Sample.Int)
module Over_prod = Req.Over (Misc.Sum) (Sample.Int)
module Under_sum = Req.Under (Misc.Sum) (Sample.Int)
module Under_prod = Req.Under (Misc.Sum) (Sample.Int)

module Over_sum_applicative_suite =
  Preface.Qcheck.Applicative.Suite (Over_sum.Req) (Over_sum.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Under_sum_applicative_suite =
  Preface.Qcheck.Applicative.Suite (Under_sum.Req) (Under_sum.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Over_prod_applicative_suite =
  Preface.Qcheck.Applicative.Suite (Over_prod.Req) (Over_prod.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Under_prod_applicative_suite =
  Preface.Qcheck.Applicative.Suite (Under_prod.Req) (Under_prod.Applicative)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Over_sum_selective_suite =
  Preface.Qcheck.Selective.Suite (Over_sum.Req) (Over_sum.Selective)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Under_sum_selective_suite =
  Preface.Qcheck.Selective.Suite (Under_sum.Req) (Under_sum.Selective)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Over_prod_selective_suite =
  Preface.Qcheck.Selective.Suite (Over_prod.Req) (Over_prod.Selective)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

module Under_prod_selective_suite =
  Preface.Qcheck.Selective.Suite (Under_prod.Req) (Under_prod.Selective)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)

let cases ~count =
  Util.with_alcotest ~count
    [
      ("Over Sum Applicative", Over_sum_applicative_suite.tests)
    ; ("Over Sum Selective", Over_sum_selective_suite.tests)
    ; ("Under Sum Applicative", Under_sum_applicative_suite.tests)
    ; ("Under Sum Selective", Under_sum_selective_suite.tests)
    ; ("Over Prod Applicative", Over_prod_applicative_suite.tests)
    ; ("Over Prod Selective", Over_prod_selective_suite.tests)
    ; ("Under Prod Applicative", Under_prod_applicative_suite.tests)
    ; ("Under Prod Selective", Under_prod_selective_suite.tests)
    ]
;;
