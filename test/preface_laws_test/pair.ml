module Bifunctor_suite =
  Preface.Qcheck.Bifunctor.Suite (Req.Pair) (Preface.Pair.Bifunctor)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)
    (Sample.Int)
    (Sample.String)

let cases ~count =
  Util.with_alcotest ~count [ ("Pair Bifunctor", Bifunctor_suite.tests) ]
;;
