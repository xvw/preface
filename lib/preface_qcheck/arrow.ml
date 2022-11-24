open QCheck2

module Suite
    (R : Model.PROFUNCTORIAL)
    (P : Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0) =
struct
  module Category = Category.Suite (R) (P) (A) (B) (C) (D)
  module Laws = Preface_laws.Arrow.For (P)

  let arrow_1 count =
    let generator = Gen.unit
    and input = R.input A.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.arrow_1
      (fun lhs rhs ((), x) ->
        let left = lhs ()
        and right = rhs () in
        R.run_equality x (R.equal A.equal) left right )
  ;;

  let arrow_2 count =
    let generator =
      Gen.tup2 (fun1 A.observable B.generator) (fun1 B.observable C.generator)
    and input = R.input A.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.arrow_2
      (fun lhs rhs ((f, g), x) ->
        let f = Fn.apply f
        and g = Fn.apply g in
        let left = lhs f g
        and right = rhs f g in
        R.run_equality x (R.equal C.equal) left right )
  ;;

  let arrow_3 count =
    let generator = fun1 A.observable B.generator
    and input = R.input (Gen.tup2 A.generator C.generator) in
    Util.test ~count (Gen.tup2 generator input) Laws.arrow_3
      (fun lhs rhs (f, x) ->
        let f = Fn.apply f in
        let left = lhs f
        and right = rhs f in
        R.run_equality x (R.equal (Util.equal_pair B.equal C.equal)) left right )
  ;;

  let arrow_4 count =
    let generator =
      Gen.tup2
        (R.generator A.observable B.generator)
        (R.generator B.observable C.generator)
    and input = R.input (Gen.tup2 A.generator D.generator) in
    Util.test ~count (Gen.tup2 generator input) Laws.arrow_4
      (fun lhs rhs ((p1, p2), x) ->
        let pro1 = R.lift p1
        and pro2 = R.lift p2 in
        let left = lhs pro1 pro2
        and right = rhs pro1 pro2 in
        R.run_equality x (R.equal (Util.equal_pair C.equal D.equal)) left right )
  ;;

  let arrow_5 count =
    let generator = R.generator A.observable B.generator
    and input = R.input (Gen.tup2 A.generator C.generator) in
    Util.test ~count (Gen.tup2 generator input) Laws.arrow_5
      (fun lhs rhs (p, x) ->
        let pro = R.lift p in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x (R.equal B.equal) left right )
  ;;

  let arrow_6 count =
    let generator =
      Gen.tup2
        (R.generator A.observable B.generator)
        (fun1 C.observable D.generator)
    and input = R.input (Gen.tup2 A.generator C.generator) in
    Util.test ~count (Gen.tup2 generator input) Laws.arrow_6
      (fun lhs rhs ((p, f), x) ->
        let f = Fn.apply f
        and pro = R.lift p in
        let left = lhs pro f
        and right = rhs pro f in
        R.run_equality x (R.equal (Util.equal_pair B.equal D.equal)) left right )
  ;;

  let arrow_7 count =
    let generator = R.generator A.observable B.generator
    and input =
      R.input (Gen.tup2 (Gen.tup2 A.generator C.generator) D.generator)
    in
    Util.test ~count (Gen.tup2 generator input) Laws.arrow_7
      (fun lhs rhs (p, x) ->
        let pro = R.lift p in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x
          (R.equal (Util.equal_pair B.equal (Util.equal_pair C.equal D.equal)))
          left right )
  ;;

  let tests ~count =
    Category.tests ~count
    @ [
        arrow_1 count
      ; arrow_2 count
      ; arrow_3 count
      ; arrow_4 count
      ; arrow_5 count
      ; arrow_6 count
      ; arrow_7 count
      ]
  ;;
end
