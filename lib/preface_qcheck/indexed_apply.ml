open QCheck2

module Suite
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_APPLY with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (Index : Model.T0) =
struct
  module Laws = Preface_laws.Indexed_apply.For (F)
  module Functor = Indexed_functor.Suite (R) (F) (A) (B) (C) (Index)

  let print pp = Format.asprintf "%a" (R.pp pp Index.pp)

  let apply_1 count =
    let generator =
      Gen.tup2
        (R.generator Gen.unit Index.generator)
        (R.generator A.generator Index.generator)
    in
    let print =
      Print.tup2 (print (fun ppf () -> Format.fprintf ppf "()")) (print A.pp)
    in
    Util.test ~count ~print generator Laws.apply_1 (fun lhs rhs (l, r) ->
        let left = lhs l r
        and right = rhs l r in
        R.equal A.equal Index.equal left right )
  ;;

  let apply_2 count =
    let generator =
      Gen.tup2
        (R.generator A.generator Index.generator)
        (R.generator Gen.unit Index.generator)
    in
    let print =
      Print.tup2 (print A.pp) (print (fun ppf () -> Format.fprintf ppf "()"))
    in
    Util.test ~count ~print generator Laws.apply_2 (fun lhs rhs (l, r) ->
        let left = lhs l r
        and right = rhs l r in
        R.equal A.equal Index.equal left right )
  ;;

  let tests ~count = Functor.tests ~count @ [ apply_1 count; apply_2 count ]
end
