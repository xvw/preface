open QCheck2

module Suite
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_BIND with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0)
    (Index : Model.T0) =
struct
  module Functor = Indexed_functor.Suite (R) (F) (A) (B) (C) (Index)
  module Laws = Preface_laws.Indexed_bind.For (F)

  let print pp = Format.asprintf "%a" (R.pp pp Index.pp)

  let bind_1 count =
    let generator =
      R.generator
        (R.generator (R.generator A.generator Index.generator) Index.generator)
        Index.generator
    in
    let print = print (R.pp (R.pp A.pp Index.pp) Index.pp) in
    Util.test ~count ~print generator Laws.bind_1 (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal Index.equal left right )
  ;;

  let bind_2 count =
    let generator =
      Gen.tup2
        (fun1 A.observable B.generator)
        (R.generator (R.generator A.generator Index.generator) Index.generator)
    in
    let print (_, x) = print (R.pp A.pp Index.pp) x in
    Util.test ~count ~print generator Laws.bind_2 (fun lhs rhs (ff, x) ->
        let f = Fn.apply ff in
        let left = lhs f x
        and right = rhs f x in
        R.equal B.equal Index.equal left right )
  ;;

  let bind_3 count =
    let generator =
      Gen.tup3
        (R.generator A.generator Index.generator)
        (fun1 A.observable (R.generator B.generator Index.generator))
        (fun1 B.observable (R.generator C.generator Index.generator))
    in
    let print (x, _, _) = print A.pp x in
    Util.test ~count ~print generator Laws.bind_3 (fun lhs rhs (x, ff, gg) ->
        let f = Fn.apply ff
        and g = Fn.apply gg in
        let left = lhs x f g
        and right = rhs x f g in
        R.equal C.equal Index.equal left right )
  ;;

  let bind_4 count =
    let generator =
      Gen.tup4
        (fun1 A.observable (R.generator B.generator Index.generator))
        (fun1 B.observable (R.generator C.generator Index.generator))
        (fun1 C.observable (R.generator D.generator Index.generator))
        A.generator
    in
    let print (_, _, _, x) = Format.asprintf "%a" A.pp x in
    Util.test ~count ~print generator Laws.bind_4
      (fun lhs rhs (ff, gg, hh, x) ->
        let f = Fn.apply ff
        and g = Fn.apply gg
        and h = Fn.apply hh in
        let left = lhs f g h x
        and right = rhs f g h x in
        R.equal D.equal Index.equal left right )
  ;;

  let tests ~count =
    Functor.tests ~count
    @ [ bind_1 count; bind_2 count; bind_3 count; bind_4 count ]
  ;;
end
