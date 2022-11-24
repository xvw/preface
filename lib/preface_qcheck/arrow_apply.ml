open QCheck2

module Suite
    (R : Model.PROFUNCTORIAL)
    (P : Preface_specs.ARROW_APPLY with type ('a, 'b) t = ('a, 'b) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0) =
struct
  module Arrow = Arrow.Suite (R) (P) (A) (B) (C) (D)
  module Laws = Preface_laws.Arrow_apply.For (P)

  let arrow_apply_1 count =
    let generator = Gen.unit
    and input = R.input (Gen.tup2 A.generator B.generator) in
    Util.test ~count (Gen.tup2 generator input) Laws.arrow_apply_1
      (fun lhs rhs ((), x) ->
        let left = lhs ()
        and right = rhs () in
        R.run_equality x (R.equal (Util.equal_pair A.equal B.equal)) left right )
  ;;

  let arrow_apply_2 count =
    let generator = R.generator A.observable B.generator
    and input =
      R.input (Gen.tup2 (R.generator B.observable C.generator) A.generator)
    in
    Util.test ~count (Gen.tup2 generator input) Laws.arrow_apply_2
      (fun lhs rhs (p, x) ->
        let pro = R.lift p
        and x = R.map_input (fun (p, x) -> (R.lift p, x)) x in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x (R.equal C.equal) left right )
  ;;

  let arrow_apply_3 count =
    let generator = R.generator A.observable B.generator
    and input =
      R.input (Gen.tup2 (R.generator C.observable A.generator) C.generator)
    in
    Util.test ~count (Gen.tup2 generator input) Laws.arrow_apply_3
      (fun lhs rhs (p, x) ->
        let pro = R.lift p
        and x = R.map_input (fun (p, x) -> (R.lift p, x)) x in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x (R.equal B.equal) left right )
  ;;

  let tests ~count =
    Arrow.tests ~count
    @ [ arrow_apply_1 count; arrow_apply_2 count; arrow_apply_3 count ]
  ;;
end
