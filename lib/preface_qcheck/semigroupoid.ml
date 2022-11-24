open QCheck2

module Suite
    (R : Model.PROFUNCTORIAL)
    (P : Preface_specs.SEMIGROUPOID with type ('a, 'b) t = ('a, 'b) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0) =
struct
  module Laws = Preface_laws.Semigroupoid.For (P)

  let semigroupoid_1 count =
    let generator =
      Gen.tup3
        (R.generator A.observable B.generator)
        (R.generator C.observable A.generator)
        (R.generator D.observable C.generator)
    and input = R.input D.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.semigroupoid_1
      (fun lhs rhs ((p1, p2, p3), x) ->
        let pro1 = R.lift p1
        and pro2 = R.lift p2
        and pro3 = R.lift p3 in
        let left = lhs pro1 pro2 pro3
        and right = rhs pro1 pro2 pro3 in
        R.run_equality x (R.equal B.equal) left right )
  ;;

  let tests ~count = [ semigroupoid_1 count ]
end
