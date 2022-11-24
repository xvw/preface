open QCheck2

module Suite
    (R : Model.PROFUNCTORIAL)
    (P : Preface_specs.CLOSED with type ('a, 'b) t = ('a, 'b) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0)
    (E : Model.T0)
    (F : Model.T0) =
struct
  module Profunctor = Profunctor.Suite (R) (P) (A) (B) (C) (D) (E) (F)
  module Laws = Preface_laws.Closed.For (P)

  let closed_1 count =
    let generator =
      Gen.tup2
        (fun1 A.observable B.generator)
        (R.generator C.observable D.generator)
    and input = R.input (fun1 B.observable C.generator)
    and final = A.generator in
    Util.test ~count (Gen.tup3 generator input final) Laws.closed_1
      (fun lhs rhs ((f, p), x, final) ->
        let f = Fn.apply f
        and input = R.map_input Fn.apply x
        and pro = R.lift p in
        let left = lhs f pro
        and right = rhs f pro in
        let equal l r =
          let a = R.run_functional_output l final
          and b = R.run_functional_output r final in
          R.equal D.equal a b
        in
        R.run_equality input equal left right )
  ;;

  let closed_2 count =
    let generator = R.generator A.observable B.generator
    and input = R.input (fun2 C.observable D.observable A.generator)
    and final = Gen.tup2 C.generator D.generator in
    Util.test ~count (Gen.tup3 generator input final) Laws.closed_2
      (fun lhs rhs (p, x, (final_1, final_2)) ->
        let pro = R.lift p
        and input = R.map_input Fn.apply x in
        let left = lhs pro
        and right = rhs pro in
        let equal l r =
          let a =
            R.(run_functional_output (run_functional_output l final_1) final_2)
          and b =
            R.(run_functional_output (run_functional_output r final_1) final_2)
          in
          R.equal B.equal a b
        in
        R.run_equality input equal left right )
  ;;

  let closed_3 count =
    let generator = R.generator A.observable B.generator
    and input = R.input A.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.closed_3
      (fun lhs rhs (p, x) ->
        let pro = R.lift p in
        let left = lhs pro
        and right = rhs pro in
        R.run_equality x (R.equal B.equal) left right )
  ;;

  let tests ~count =
    Profunctor.tests ~count @ [ closed_1 count; closed_2 count; closed_3 count ]
  ;;
end
