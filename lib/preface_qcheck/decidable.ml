open QCheck2

module Suite
    (R : Model.CONTRAVARIANT_1)
    (F : Preface_specs.DECIDABLE with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) =
struct
  module Laws = Preface_laws.Decidable.For (F)
  module Divisible = Divisible.Suite (R) (F) (A) (B) (C)

  let decidable_1 count =
    let generator = R.generator A.observable
    and input = R.input A.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.decidable_1
      (fun lhs rhs (f, x) ->
        let decidable = R.lift f
        and f : 'a -> Preface_core.Void.t = function _ -> assert false in
        let left = lhs f decidable
        and right = rhs f decidable in
        R.run_equality x left right )
  ;;

  let decidable_2 count =
    let generator = R.generator A.observable
    and input = R.input A.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.decidable_2
      (fun lhs rhs (f, x) ->
        let decidable = R.lift f
        and f : 'a -> Preface_core.Void.t = function _ -> assert false in
        let left = lhs f decidable
        and right = rhs f decidable in
        R.run_equality x left right )
  ;;

  let tests ~count =
    Divisible.tests ~count @ [ decidable_1 count; decidable_2 count ]
  ;;
end
