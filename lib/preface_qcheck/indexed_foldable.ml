open QCheck2

module Suite
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_FOLDABLE
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (M : Preface_specs.MONOID with type t = A.t)
    (Index : Model.T0) =
struct
  module Laws = Preface_laws.Indexed_foldable.For (F)

  let print pp = Format.asprintf "%a" (R.pp pp Index.pp)

  let foldable_1 count =
    let generator =
      Gen.tup3
        (fun2 B.observable A.observable A.generator)
        A.generator
        (R.generator B.generator Index.generator)
    in
    let print (_, x, y) =
      Print.tup2 (Format.asprintf "%a" A.pp) (print B.pp) (x, y)
    in
    Util.test ~count ~print generator Laws.foldable_1 (fun lhs rhs (ff, x, y) ->
        let f = Fn.apply ff in
        let left = lhs (module A) f x y
        and right = rhs (module A) f x y in
        A.equal left right )
  ;;

  let foldable_2 count =
    let generator =
      Gen.tup3
        (fun2 A.observable B.observable A.generator)
        A.generator
        (R.generator B.generator Index.generator)
    in
    let print (_, x, y) =
      Print.tup2 (Format.asprintf "%a" A.pp) (print B.pp) (x, y)
    in
    Util.test ~count ~print generator Laws.foldable_2 (fun lhs rhs (ff, x, y) ->
        let f = Fn.apply ff in
        let left = lhs (module A) f x y
        and right = rhs (module A) f x y in
        A.equal left right )
  ;;

  let foldable_3 count =
    let generator = R.generator A.generator Index.generator in
    let print = print A.pp in
    Util.test ~count ~print generator Laws.foldable_3 (fun lhs rhs x ->
        let left = lhs (module M) x
        and right = rhs (module M) x in
        A.equal left right )
  ;;

  let tests ~count = [ foldable_1 count; foldable_2 count; foldable_3 count ]
end
