open QCheck2

module Suite
    (R : Model.COVARIANT_1)
    (F : Preface_specs.APPLY with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) =
struct
  module Laws = Preface_laws.Apply.For (F)
  module Functor = Functor.Suite (R) (F) (A) (B) (C)

  let print pp = Format.asprintf "%a" (R.pp pp)

  let apply_1 count =
    let generator = Gen.tup2 (R.generator Gen.unit) (R.generator A.generator) in
    let print =
      Print.tup2 (print (fun ppf () -> Format.fprintf ppf "()")) (print A.pp)
    in
    Util.test ~count ~print generator Laws.apply_1 (fun lhs rhs (l, r) ->
        let left = lhs l r
        and right = rhs l r in
        R.equal A.equal left right )
  ;;

  let apply_2 count =
    let generator = Gen.tup2 (R.generator A.generator) (R.generator Gen.unit) in
    let print =
      Print.tup2 (print A.pp) (print (fun ppf () -> Format.fprintf ppf "()"))
    in
    Util.test ~count ~print generator Laws.apply_2 (fun lhs rhs (l, r) ->
        let left = lhs l r
        and right = rhs l r in
        R.equal A.equal left right )
  ;;

  let tests ~count = Functor.tests ~count @ [ apply_1 count; apply_2 count ]
end
