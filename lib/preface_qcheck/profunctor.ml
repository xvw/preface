open QCheck2

module Suite
    (R : Model.PROFUNCTORIAL)
    (P : Preface_specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0)
    (E : Model.T0)
    (F : Model.T0) =
struct
  module Laws = Preface_laws.Profunctor.For (P)

  let profunctor_1 count =
    let generator = R.generator A.observable B.generator
    and input = R.input A.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.profunctor_1
      (fun lhs rhs (f, x) ->
        let pro = R.lift f in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x (R.equal B.equal) left right )
  ;;

  let profunctor_2 count =
    let generator = R.generator A.observable B.generator
    and input = R.input A.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.profunctor_2
      (fun lhs rhs (f, x) ->
        let pro = R.lift f in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x (R.equal B.equal) left right )
  ;;

  let profunctor_3 count =
    let generator = R.generator A.observable B.generator
    and input = R.input A.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.profunctor_3
      (fun lhs rhs (f, x) ->
        let pro = R.lift f in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x (R.equal B.equal) left right )
  ;;

  let profunctor_4 count =
    let generator =
      Gen.tup3
        (fun1 A.observable B.generator)
        (fun1 C.observable D.generator)
        (R.generator B.observable C.generator)
    and input = R.input A.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.profunctor_4
      (fun lhs rhs ((f, g, p), x) ->
        let f = Fn.apply f
        and g = Fn.apply g
        and pro = R.lift p in
        let left = lhs f g pro
        and right = rhs f g pro in
        R.run_equality x (R.equal D.equal) left right )
  ;;

  let profunctor_5 count =
    let generator =
      Gen.tup5
        (fun1 A.observable B.generator)
        (fun1 C.observable A.generator)
        (fun1 D.observable E.generator)
        (fun1 F.observable D.generator)
        (R.generator B.observable F.generator)
    and input = R.input C.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.profunctor_5
      (fun lhs rhs ((f, g, h, i, p), x) ->
        let f = Fn.apply f
        and g = Fn.apply g
        and h = Fn.apply h
        and i = Fn.apply i
        and pro = R.lift p in
        let left = lhs f g h i pro
        and right = rhs f g h i pro in
        R.run_equality x (R.equal E.equal) left right )
  ;;

  let profunctor_6 count =
    let generator =
      Gen.tup3
        (fun1 A.observable B.generator)
        (fun1 C.observable A.generator)
        (R.generator B.observable D.generator)
    and input = R.input C.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.profunctor_6
      (fun lhs rhs ((f, g, p), x) ->
        let f = Fn.apply f
        and g = Fn.apply g
        and pro = R.lift p in
        let left = lhs f g pro
        and right = rhs f g pro in
        R.run_equality x (R.equal D.equal) left right )
  ;;

  let profunctor_7 count =
    let generator =
      Gen.tup3
        (fun1 A.observable B.generator)
        (fun1 C.observable A.generator)
        (R.generator D.observable C.generator)
    and input = R.input D.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.profunctor_7
      (fun lhs rhs ((f, g, p), x) ->
        let f = Fn.apply f
        and g = Fn.apply g
        and pro = R.lift p in
        let left = lhs f g pro
        and right = rhs f g pro in
        R.run_equality x (R.equal B.equal) left right )
  ;;

  let tests ~count =
    [
      profunctor_1 count
    ; profunctor_2 count
    ; profunctor_3 count
    ; profunctor_4 count
    ; profunctor_5 count
    ; profunctor_6 count
    ; profunctor_7 count
    ]
  ;;
end
