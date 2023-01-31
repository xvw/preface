open QCheck2

module Suite
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_COMONAD
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0)
    (Index : Model.T0) =
struct
  module Functor = Indexed_functor.Suite (R) (F) (A) (B) (C) (Index)
  module Laws = Preface_laws.Indexed_comonad.For (F)

  let print pp = Format.asprintf "%a" (R.pp pp Index.pp)

  let comonad_1 count =
    let generator = R.generator A.generator Index.generator in
    let print = print A.pp in
    Util.test ~count ~print generator Laws.comonad_1 (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal Index.equal left right )
  ;;

  let comonad_2 count =
    let generator =
      Gen.tup2
        (fun1 (R.observable A.observable Index.observable) B.generator)
        (R.generator A.generator Index.generator)
    in
    let print (_, x) = print A.pp x in
    Util.test ~count ~print generator Laws.comonad_2 (fun lhs rhs (ff, x) ->
        let f = Fn.apply ff in
        let left = lhs f x
        and right = rhs f x in
        B.equal left right )
  ;;

  let comonad_3 count =
    let generator =
      Gen.tup3
        (fun1 (R.observable A.observable Index.observable) B.generator)
        (fun1 (R.observable C.observable Index.observable) A.generator)
        (R.generator C.generator Index.generator)
    in
    let print (_, _, x) = print C.pp x in
    Util.test ~count ~print generator Laws.comonad_3 (fun lhs rhs (ff, gg, x) ->
        let f = Fn.apply ff
        and g = Fn.apply gg in
        let left = lhs f g x
        and right = rhs f g x in
        R.equal B.equal Index.equal left right )
  ;;

  let comonad_4 count =
    let generator =
      Gen.tup2
        (fun1 (R.observable A.observable Index.observable) B.generator)
        (R.generator A.generator Index.generator)
    in
    let print (_, x) = print A.pp x in
    Util.test ~count ~print generator Laws.comonad_4 (fun lhs rhs (ff, x) ->
        let f = Fn.apply ff in
        let left = lhs f x
        and right = rhs f x in
        B.equal left right )
  ;;

  let comonad_5 count =
    let generator =
      Gen.tup2
        (fun1 (R.observable A.observable Index.observable) B.generator)
        (R.generator A.generator Index.generator)
    in
    let print (_, x) = print A.pp x in
    Util.test ~count ~print generator Laws.comonad_5 (fun lhs rhs (ff, x) ->
        let f = Fn.apply ff in
        let left = lhs f x
        and right = rhs f x in
        B.equal left right )
  ;;

  let comonad_6 count =
    let generator =
      Gen.tup4
        (fun1 (R.observable A.observable Index.observable) B.generator)
        (fun1 (R.observable B.observable Index.observable) C.generator)
        (fun1 (R.observable C.observable Index.observable) D.generator)
        (R.generator A.generator Index.generator)
    in
    let print (_, _, _, x) = print A.pp x in
    Util.test ~count ~print generator Laws.comonad_6
      (fun lhs rhs (ff, gg, hh, x) ->
        let f = Fn.apply ff
        and g = Fn.apply gg
        and h = Fn.apply hh in
        let left = lhs f g h x
        and right = rhs f g h x in
        D.equal left right )
  ;;

  let comonad_7 count =
    let generator = R.generator A.generator Index.generator in
    let print = print A.pp in
    Util.test ~count ~print generator Laws.comonad_7 (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal Index.equal left right )
  ;;

  let comonad_8 count =
    let generator = R.generator A.generator Index.generator in
    let print = print A.pp in
    Util.test ~count ~print generator Laws.comonad_8 (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal Index.equal left right )
  ;;

  let comonad_9 count =
    let generator = R.generator A.generator Index.generator in
    let print = print A.pp in
    Util.test ~count ~print generator Laws.comonad_9 (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal
          (R.equal (R.equal A.equal Index.equal) Index.equal)
          Index.equal left right )
  ;;

  let comonad_10 count =
    let generator =
      Gen.tup2
        (fun1 (R.observable A.observable Index.observable) B.generator)
        (R.generator A.generator Index.generator)
    in
    let print (_, x) = print A.pp x in
    Util.test ~count ~print generator Laws.comonad_10 (fun lhs rhs (ff, x) ->
        let f = Fn.apply ff in
        let left = lhs f x
        and right = rhs f x in
        R.equal B.equal Index.equal left right )
  ;;

  let comonad_11 count =
    let generator = R.generator A.generator Index.generator in
    let print = print A.pp in
    Util.test ~count ~print generator Laws.comonad_11 (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal (R.equal A.equal Index.equal) Index.equal left right )
  ;;

  let comonad_12 count =
    let generator =
      Gen.tup2
        (fun1 A.observable B.generator)
        (R.generator A.generator Index.generator)
    in
    let print (_, x) = print A.pp x in
    Util.test ~count ~print generator Laws.comonad_12 (fun lhs rhs (ff, x) ->
        let f = Fn.apply ff in
        let left = lhs f x
        and right = rhs f x in
        R.equal B.equal Index.equal left right )
  ;;

  let tests ~count =
    Functor.tests ~count
    @ [
        comonad_1 count
      ; comonad_2 count
      ; comonad_3 count
      ; comonad_4 count
      ; comonad_5 count
      ; comonad_6 count
      ; comonad_7 count
      ; comonad_8 count
      ; comonad_9 count
      ; comonad_10 count
      ; comonad_11 count
      ; comonad_12 count
      ]
  ;;
end
