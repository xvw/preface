open QCheck2

module Suite
    (R : Model.CONTRAVARIANT_1)
    (F : Preface_specs.CONTRAVARIANT with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) =
struct
  module Laws = Preface_laws.Contravariant.For (F)

  let contravariant_1 count =
    let generator = R.generator A.observable
    and input = R.input A.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.contravariant_1
      (fun lhs rhs (f, x) ->
        let contra = R.lift f in
        let left = lhs contra
        and right = rhs contra in
        R.run_equality x left right )
  ;;

  let contravariant_2 count =
    let generator =
      Gen.tup3
        (fun1 A.observable B.generator)
        (fun1 B.observable C.generator)
        (R.generator C.observable)
    and input = R.input A.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.contravariant_2
      (fun lhs rhs ((ff, gg, ccontra), x) ->
        let f = Fn.apply ff
        and g = Fn.apply gg
        and contra = R.lift ccontra in
        let left = lhs f g contra
        and right = rhs f g contra in
        R.run_equality x left right )
  ;;

  let tests ~count = [ contravariant_1 count; contravariant_2 count ]
end
