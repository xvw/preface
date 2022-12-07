open QCheck2

module Suite
    (R : Model.PROFUNCTORIAL)
    (P : Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0)
    (E : Model.T0)
    (F : Model.T0) =
struct
  module Profunctor = Profunctor.Suite (R) (P) (A) (B) (C) (D) (E) (F)
  module Laws = Preface_laws.Strong.For (P)

  let strong_1 count =
    let generator = R.generator A.observable B.generator
    and input = R.input (Gen.tup2 A.generator C.generator) in
    Util.test ~count (Gen.tup2 generator input) Laws.strong_1
      (fun lhs rhs (p, x) ->
        let pro = R.lift p in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x (R.equal (Util.equal_pair B.equal C.equal)) left right )
  ;;

  let strong_2 count =
    let generator = R.generator A.observable B.generator
    and input = R.input (Gen.tup2 A.generator C.generator) in
    Util.test ~count (Gen.tup2 generator input) Laws.strong_2
      (fun lhs rhs (p, x) ->
        let pro = R.lift p in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x (R.equal B.equal) left right )
  ;;

  let strong_3 count =
    let generator =
      Gen.tup2
        (fun1 A.observable B.generator)
        (R.generator C.observable D.generator)
    and input = R.input (Gen.tup2 C.generator A.generator) in
    Util.test ~count (Gen.tup2 generator input) Laws.strong_3
      (fun lhs rhs ((f, p), x) ->
        let f = Fn.apply f
        and pro = R.lift p in
        let left = lhs f pro
        and right = rhs f pro in
        R.run_equality x (R.equal (Util.equal_pair D.equal B.equal)) left right )
  ;;

  let strong_4 count =
    let generator = R.generator A.observable B.generator
    and input =
      R.input (Gen.tup2 (Gen.tup2 A.generator C.generator) D.generator)
    in
    Util.test ~count (Gen.tup2 generator input) Laws.strong_4
      (fun lhs rhs (p, x) ->
        let pro = R.lift p in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x
          (R.equal (Util.equal_pair (Util.equal_pair B.equal C.equal) D.equal))
          left right )
  ;;

  let strong_5 count =
    let generator = R.generator A.observable B.generator
    and input = R.input (Gen.tup2 C.generator A.generator) in
    Util.test ~count (Gen.tup2 generator input) Laws.strong_5
      (fun lhs rhs (p, x) ->
        let pro = R.lift p in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x (R.equal (Util.equal_pair C.equal B.equal)) left right )
  ;;

  let strong_6 count =
    let generator = R.generator A.observable B.generator
    and input = R.input (Gen.tup2 C.generator A.generator) in
    Util.test ~count (Gen.tup2 generator input) Laws.strong_6
      (fun lhs rhs (p, x) ->
        let pro = R.lift p in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x (R.equal B.equal) left right )
  ;;

  let strong_7 count =
    let generator =
      Gen.tup2
        (fun1 A.observable B.generator)
        (R.generator C.observable D.generator)
    and input = R.input (Gen.tup2 A.generator C.generator) in
    Util.test ~count (Gen.tup2 generator input) Laws.strong_7
      (fun lhs rhs ((f, p), x) ->
        let f = Fn.apply f
        and pro = R.lift p in
        let left = lhs f pro
        and right = rhs f pro in
        R.run_equality x (R.equal (Util.equal_pair B.equal D.equal)) left right )
  ;;

  let strong_8 count =
    let generator = R.generator A.observable B.generator
    and input =
      R.input (Gen.tup2 C.generator (Gen.tup2 D.generator A.generator))
    in
    Util.test ~count (Gen.tup2 generator input) Laws.strong_8
      (fun lhs rhs (p, x) ->
        let pro = R.lift p in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x
          (R.equal Util.(equal_pair C.equal (equal_pair D.equal B.equal)))
          left right )
  ;;

  let tests ~count =
    Profunctor.tests ~count
    @ [
        strong_1 count
      ; strong_2 count
      ; strong_3 count
      ; strong_4 count
      ; strong_5 count
      ; strong_6 count
      ; strong_7 count
      ; strong_8 count
      ]
  ;;
end
