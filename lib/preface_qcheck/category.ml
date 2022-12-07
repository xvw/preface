open QCheck2

module Suite
    (R : Model.PROFUNCTORIAL)
    (P : Preface_specs.CATEGORY with type ('a, 'b) t = ('a, 'b) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0) =
struct
  module Semigroupoid = Semigroupoid.Suite (R) (P) (A) (B) (C) (D)
  module Laws = Preface_laws.Category.For (P)

  let category_1 count =
    let generator = R.generator A.observable B.generator
    and input = R.input A.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.category_1
      (fun lhs rhs (p, x) ->
        let pro = R.lift p in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x (R.equal B.equal) left right )
  ;;

  let category_2 count =
    let generator = R.generator A.observable B.generator
    and input = R.input A.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.category_2
      (fun lhs rhs (p, x) ->
        let pro = R.lift p in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x (R.equal B.equal) left right )
  ;;

  let tests ~count =
    Semigroupoid.tests ~count @ [ category_1 count; category_2 count ]
  ;;
end
