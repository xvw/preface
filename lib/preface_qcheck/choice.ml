open QCheck2

module Suite
    (R : Model.PROFUNCTORIAL)
    (P : Preface_specs.CHOICE with type ('a, 'b) t = ('a, 'b) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0)
    (E : Model.T0)
    (F : Model.T0) =
struct
  module Profunctor = Profunctor.Suite (R) (P) (A) (B) (C) (D) (E) (F)
  module Laws = Preface_laws.Choice.For (P)

  let choice_1 count =
    let generator = R.generator A.observable B.generator
    and input = R.input (Util.gen_either A.generator C.generator) in
    Util.test ~count (Gen.tup2 generator input) Laws.choice_1
      (fun lhs rhs (p, x) ->
        let pro = R.lift p in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x
          (R.equal (Util.equal_either B.equal C.equal))
          left right )
  ;;

  let choice_2 count =
    let generator = R.generator A.observable B.generator
    and input = R.input A.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.choice_2
      (fun lhs rhs (p, x) ->
        let pro = R.lift p in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x
          (R.equal (Util.equal_either B.equal C.equal))
          left right )
  ;;

  let choice_3 count =
    let generator =
      Gen.tup2
        (fun1 A.observable B.generator)
        (R.generator C.observable D.generator)
    and input = R.input (Util.gen_either C.generator A.generator) in
    Util.test ~count (Gen.tup2 generator input) Laws.choice_3
      (fun lhs rhs ((f, p), x) ->
        let f = Fn.apply f
        and pro = R.lift p in
        let left = lhs f pro
        and right = rhs f pro in
        R.run_equality x
          (R.equal (Util.equal_either D.equal B.equal))
          left right )
  ;;

  let choice_4 count =
    let generator = R.generator A.observable B.generator
    and input =
      R.input
        (Util.gen_either (Util.gen_either A.generator C.generator) D.generator)
    in
    Util.test ~count (Gen.tup2 generator input) Laws.choice_4
      (fun lhs rhs (p, x) ->
        let pro = R.lift p in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x
          (R.equal Util.(equal_either (equal_either B.equal C.equal) D.equal))
          left right )
  ;;

  let choice_5 count =
    let generator = R.generator A.observable B.generator
    and input = R.input (Util.gen_either C.generator A.generator) in
    Util.test ~count (Gen.tup2 generator input) Laws.choice_5
      (fun lhs rhs (p, x) ->
        let pro = R.lift p in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x
          (R.equal (Util.equal_either C.equal B.equal))
          left right )
  ;;

  let choice_6 count =
    let generator = R.generator A.observable B.generator
    and input = R.input A.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.choice_6
      (fun lhs rhs (p, x) ->
        let pro = R.lift p in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x
          (R.equal (Util.equal_either C.equal B.equal))
          left right )
  ;;

  let choice_7 count =
    let generator =
      Gen.tup2
        (fun1 A.observable B.generator)
        (R.generator C.observable D.generator)
    and input = R.input (Util.gen_either A.generator C.generator) in
    Util.test ~count (Gen.tup2 generator input) Laws.choice_7
      (fun lhs rhs ((f, p), x) ->
        let f = Fn.apply f
        and pro = R.lift p in
        let left = lhs f pro
        and right = rhs f pro in
        R.run_equality x
          (R.equal (Util.equal_either B.equal D.equal))
          left right )
  ;;

  let choice_8 count =
    let generator = R.generator A.observable B.generator
    and input =
      R.input Util.(gen_either C.generator (gen_either D.generator A.generator))
    in
    Util.test ~count (Gen.tup2 generator input) Laws.choice_8
      (fun lhs rhs (p, x) ->
        let pro = R.lift p in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x
          (R.equal Util.(equal_either C.equal (equal_either D.equal B.equal)))
          left right )
  ;;

  let tests ~count =
    Profunctor.tests ~count
    @ [
        choice_1 count
      ; choice_2 count
      ; choice_3 count
      ; choice_4 count
      ; choice_5 count
      ; choice_6 count
      ; choice_7 count
      ; choice_8 count
      ]
  ;;
end
