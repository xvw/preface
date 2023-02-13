open QCheck2

module Suite
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_APPLICATIVE
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (Index : Model.T0) =
struct
  module Laws = Preface_laws.Indexed_applicative.For (F)
  module Apply = Indexed_apply.Suite (R) (F) (A) (B) (C) (Index)

  let print pp = Format.asprintf "%a" (R.pp pp Index.pp)

  let applicative_1 count =
    let generator = R.generator A.generator Index.generator in
    let print = print A.pp in
    Util.test ~count ~print generator Laws.applicative_1 (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal Index.equal left right )
  ;;

  let applicative_2 count =
    let generator = Gen.tup2 (fun1 A.observable B.generator) A.generator in
    let print (_, x) = Format.asprintf "%a" A.pp x in
    Util.test ~count ~print generator Laws.applicative_2 (fun lhs rhs (ff, x) ->
        let f = Fn.apply ff in
        let left = lhs f x
        and right = rhs f x in
        R.equal B.equal Index.equal left right )
  ;;

  let applicative_3 count =
    let generator =
      Gen.tup2
        (R.generator (fun1 A.observable B.generator) Index.generator)
        A.generator
    in
    let print (_, x) = Format.asprintf "%a" A.pp x in
    Util.test ~count ~print generator Laws.applicative_3 (fun lhs rhs (ff, x) ->
        let f = F.(Fn.apply <$> ff) in
        let left = lhs f x
        and right = rhs f x in
        R.equal B.equal Index.equal left right )
  ;;

  let applicative_4 count =
    let generator =
      Gen.tup3
        (R.generator (fun1 A.observable B.generator) Index.generator)
        (R.generator (fun1 C.observable A.generator) Index.generator)
        (R.generator C.generator Index.generator)
    in
    let print (_, _, x) = print C.pp x in
    Util.test ~count ~print generator Laws.applicative_4
      (fun lhs rhs (ff, gg, x) ->
        let f = F.(Fn.apply <$> ff)
        and g = F.(Fn.apply <$> gg) in
        let left = lhs f g x
        and right = rhs f g x in
        R.equal B.equal Index.equal left right )
  ;;

  let applicative_5 count =
    let generator =
      Gen.tup2
        (fun1 A.observable B.generator)
        (R.generator A.generator Index.generator)
    in
    let print (_, x) = print A.pp x in
    Util.test ~count ~print generator Laws.applicative_5 (fun lhs rhs (ff, x) ->
        let f = Fn.apply ff in
        let left = lhs f x
        and right = rhs f x in
        R.equal B.equal Index.equal left right )
  ;;

  let tests ~count =
    Apply.tests ~count
    @ [
        applicative_1 count
      ; applicative_2 count
      ; applicative_3 count
      ; applicative_4 count
      ; applicative_5 count
      ]
  ;;
end
