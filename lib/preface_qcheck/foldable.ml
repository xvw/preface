open QCheck2

module Suite
    (R : Model.COVARIANT_1)
    (F : Preface_specs.FOLDABLE with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (M : Preface_specs.MONOID with type t = A.t) =
struct
  module Laws = Preface_laws.Foldable.For (F)

  let print pp = Format.asprintf "%a" (R.pp pp)

  let foldable_1 count =
    let generator =
      Gen.tup3
        (fun2 B.observable A.observable A.generator)
        A.generator (R.generator B.generator)
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
        A.generator (R.generator B.generator)
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
    let generator = R.generator A.generator in
    let print = print A.pp in
    Util.test ~count ~print generator Laws.foldable_3 (fun lhs rhs x ->
        let left = lhs (module M) x
        and right = rhs (module M) x in
        A.equal left right )
  ;;

  let tests ~count = [ foldable_1 count; foldable_2 count; foldable_3 count ]
end
