open QCheck2

module Suite
    (R : Model.PROFUNCTORIAL)
    (P : Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0) =
struct
  module Arrow = Arrow.Suite (R) (P) (A) (B) (C) (D)
  module Laws = Preface_laws.Arrow_choice.For (P)

  let arrow_choice_1 count =
    let generator = fun1 A.observable B.generator
    and input = R.input (Util.gen_either A.generator C.generator) in
    Util.test ~count (Gen.tup2 generator input) Laws.arrow_choice_1
      (fun lhs rhs (f, x) ->
        let f = Fn.apply f in
        let left = lhs f
        and right = rhs f in
        R.run_equality x
          (R.equal (Util.equal_either B.equal C.equal))
          left right )
  ;;

  let arrow_choice_2 count =
    let generator =
      Gen.tup2
        (R.generator A.observable B.generator)
        (R.generator B.observable C.generator)
    and input = R.input (Util.gen_either A.generator D.generator) in
    Util.test ~count (Gen.tup2 generator input) Laws.arrow_choice_2
      (fun lhs rhs ((p1, p2), x) ->
        let pro1 = R.lift p1
        and pro2 = R.lift p2 in
        let left = lhs pro1 pro2
        and right = rhs pro1 pro2 in
        R.run_equality x
          (R.equal (Util.equal_either C.equal D.equal))
          left right )
  ;;

  let arrow_choice_3 count =
    let generator = R.generator A.observable B.generator
    and input = R.input A.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.arrow_choice_3
      (fun lhs rhs (p, x) ->
        let pro = R.lift p in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x
          (R.equal (Util.equal_either B.equal C.equal))
          left right )
  ;;

  let arrow_choice_4 count =
    let generator =
      Gen.tup2
        (R.generator A.observable B.generator)
        (fun1 C.observable D.generator)
    and input = R.input (Util.gen_either A.generator C.generator) in
    Util.test ~count (Gen.tup2 generator input) Laws.arrow_choice_4
      (fun lhs rhs ((p, f), x) ->
        let pro = R.lift p
        and f = Fn.apply f in
        let left = lhs pro f
        and right = rhs pro f in
        R.run_equality x
          (R.equal (Util.equal_either B.equal D.equal))
          left right )
  ;;

  let arrow_choice_5 count =
    let generator = R.generator A.observable B.generator
    and input =
      R.input
        (Util.gen_either (Util.gen_either A.generator C.generator) D.generator)
    in
    Util.test ~count (Gen.tup2 generator input) Laws.arrow_choice_5
      (fun lhs rhs (p, x) ->
        let pro = R.lift p in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x
          (R.equal
             (Util.equal_either B.equal (Util.equal_either C.equal D.equal)) )
          left right )
  ;;

  let tests ~count =
    Arrow.tests ~count
    @ [
        arrow_choice_1 count
      ; arrow_choice_2 count
      ; arrow_choice_3 count
      ; arrow_choice_4 count
      ; arrow_choice_5 count
      ]
  ;;
end
