open QCheck2

module Suite
    (R : Model.CONTRAVARIANT_1)
    (F : Preface_specs.DIVISIBLE with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) =
struct
  module Contravariant = Contravariant.Suite (R) (F) (A) (B) (C)
  module Laws = Preface_laws.Divisible.For (F)

  let divisible_1 count =
    let generator = R.generator A.observable
    and input = R.input A.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.divisible_1
      (fun lhs rhs (f, x) ->
        let contra = R.lift f in
        let left = lhs contra
        and right = rhs contra in
        R.run_equality x left right )
  ;;

  let divisible_2 count =
    let generator = R.generator A.observable
    and input = R.input A.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.divisible_2
      (fun lhs rhs (f, x) ->
        let contra = R.lift f in
        let left = lhs contra
        and right = rhs contra in
        R.run_equality x left right )
  ;;

  let divisible_3 count =
    let generator =
      Gen.tup3 (R.generator A.observable) (R.generator A.observable)
        (R.generator A.observable)
    and input = R.input A.generator in

    Util.test ~count (Gen.tup2 generator input) Laws.divisible_3
      (fun lhs rhs ((f, g, h), x) ->
        let contra1 = R.lift f in
        let contra2 = R.lift g in
        let contra3 = R.lift h in
        let left = lhs contra1 contra2 contra3
        and right = rhs contra1 contra2 contra3 in
        R.run_equality x left right )
  ;;

  let divisible_4 count =
    let generator =
      Gen.tup2
        (fun1 A.observable Gen.(tup2 B.generator C.generator))
        (R.generator B.observable)
    and input = R.input A.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.divisible_4
      (fun lhs rhs ((ff, c), x) ->
        let contra1 = R.lift c in
        let f = Fn.apply ff in
        let left = lhs f contra1
        and right = rhs f contra1 in
        R.run_equality x left right )
  ;;

  let divisible_5 count =
    let generator =
      Gen.tup2
        (fun1 A.observable Gen.(tup2 B.generator C.generator))
        (R.generator C.observable)
    and input = R.input A.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.divisible_5
      (fun lhs rhs ((ff, c), x) ->
        let contra1 = R.lift c in
        let f = Fn.apply ff in
        let left = lhs f contra1
        and right = rhs f contra1 in
        R.run_equality x left right )
  ;;

  let tests ~count =
    Contravariant.tests ~count
    @ [
        divisible_1 count
      ; divisible_2 count
      ; divisible_3 count
      ; divisible_4 count
      ; divisible_5 count
      ]
  ;;
end
