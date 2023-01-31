open QCheck2

module Suite
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_MONAD with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0)
    (Index : Model.T0) =
struct
  module Bind = Indexed_bind.Suite (R) (F) (A) (B) (C) (D) (Index)
  module Laws = Preface_laws.Indexed_monad.For (F)

  let print pp = Format.asprintf "%a" (R.pp pp Index.pp)

  let monad_1 count =
    let generator = R.generator A.generator Index.generator in
    let print = print A.pp in
    Util.test ~count ~print generator Laws.monad_1 (fun lhs rhs x ->
        let left, id_left = lhs x
        and right, id_right = rhs x in
        R.equal A.equal Index.equal left right
        && R.equal A.equal Index.equal left id_left
        && R.equal A.equal Index.equal right id_right )
  ;;

  let monad_2 count =
    let generator = Gen.tup2 (fun1 A.observable B.generator) A.generator in
    let print (_, x) = Format.asprintf "%a" A.pp x in
    Util.test ~count ~print generator Laws.monad_2 (fun lhs rhs (ff, x) ->
        let f = Fn.apply ff in
        let left = lhs f x
        and right = rhs f x in
        R.equal B.equal Index.equal left right )
  ;;

  let monad_3 count =
    let generator =
      Gen.tup2 A.generator
        (fun1 A.observable (R.generator B.generator Index.generator))
    in
    let print (x, _) = Format.asprintf "%a" A.pp x in
    Util.test ~count ~print generator Laws.monad_3 (fun lhs rhs (x, ff) ->
        let f = Fn.apply ff in
        let left = lhs x f
        and right = rhs x f in
        R.equal B.equal Index.equal left right )
  ;;

  let monad_4 count =
    let generator = R.generator A.generator Index.generator in
    let print = print A.pp in
    Util.test ~count ~print generator Laws.monad_4 (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal Index.equal left right )
  ;;

  let monad_5 count =
    let generator =
      Gen.tup2 A.generator
        (fun1 A.observable (R.generator B.generator Index.generator))
    in
    let print (x, _) = Format.asprintf "%a" A.pp x in
    Util.test ~count ~print generator Laws.monad_5 (fun lhs rhs (x, ff) ->
        let f = Fn.apply ff in
        let left = lhs x f
        and right = rhs x f in
        R.equal B.equal Index.equal left right )
  ;;

  let monad_6 count =
    let generator =
      Gen.tup2 A.generator
        (fun1 A.observable (R.generator B.generator Index.generator))
    in
    let print (x, _) = Format.asprintf "%a" A.pp x in
    Util.test ~count ~print generator Laws.monad_6 (fun lhs rhs (x, ff) ->
        let f = Fn.apply ff in
        let left = lhs x f
        and right = rhs x f in
        R.equal B.equal Index.equal left right )
  ;;

  let tests ~count =
    Bind.tests ~count
    @ [
        monad_1 count
      ; monad_2 count
      ; monad_3 count
      ; monad_4 count
      ; monad_5 count
      ; monad_6 count
      ]
  ;;
end
