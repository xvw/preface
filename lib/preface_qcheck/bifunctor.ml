open QCheck2

module Suite
    (R : Model.COVARIANT_2)
    (BF : Preface_specs.BIFUNCTOR with type ('a, 'b) t = ('a, 'b) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0)
    (E : Model.T0)
    (F : Model.T0) =
struct
  module Laws = Preface_laws.Bifunctor.For (BF)

  let print pp_a pp_b = Format.asprintf "%a" (R.pp pp_a pp_b)

  let bifunctor_1 count =
    let generator = R.generator A.generator B.generator in
    let print = print A.pp B.pp in
    Util.test ~count ~print generator Laws.bifunctor_1 (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal B.equal left right )
  ;;

  let bifunctor_2 count =
    let generator = R.generator A.generator B.generator in
    let print = print A.pp B.pp in
    Util.test ~count ~print generator Laws.bifunctor_2 (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal B.equal left right )
  ;;

  let bifunctor_3 count =
    let generator = R.generator A.generator B.generator in
    let print = print A.pp B.pp in
    Util.test ~count ~print generator Laws.bifunctor_3 (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal B.equal left right )
  ;;

  let bifunctor_4 count =
    let generator =
      Gen.tup3
        (fun1 A.observable B.generator)
        (fun1 C.observable D.generator)
        (R.generator A.generator C.generator)
    in
    let print (_, _, x) = print A.pp C.pp x in
    Util.test ~count ~print generator Laws.bifunctor_4 (fun lhs rhs (f, g, x) ->
        let f = Fn.apply f
        and g = Fn.apply g in
        let left = lhs f g x
        and right = rhs f g x in
        R.equal B.equal D.equal left right )
  ;;

  let bifunctor_5 count =
    let generator =
      Gen.tup5
        (fun1 A.observable B.generator)
        (fun1 C.observable A.generator)
        (fun1 D.observable E.generator)
        (fun1 F.observable D.generator)
        (R.generator C.generator F.generator)
    in
    let print (_, _, _, _, x) = print C.pp F.pp x in
    Util.test ~count ~print generator Laws.bifunctor_5
      (fun lhs rhs (f, g, h, i, x) ->
        let f = Fn.apply f
        and g = Fn.apply g
        and h = Fn.apply h
        and i = Fn.apply i in
        let left = lhs f g h i x
        and right = rhs f g h i x in
        R.equal B.equal E.equal left right )
  ;;

  let bifunctor_6 count =
    let generator =
      Gen.tup3
        (fun1 A.observable B.generator)
        (fun1 C.observable A.generator)
        (R.generator C.generator D.generator)
    in
    let print (_, _, x) = print C.pp D.pp x in
    Util.test ~count ~print generator Laws.bifunctor_6 (fun lhs rhs (f, g, x) ->
        let f = Fn.apply f
        and g = Fn.apply g in
        let left = lhs f g x
        and right = rhs f g x in
        R.equal B.equal D.equal left right )
  ;;

  let bifunctor_7 count =
    let generator =
      Gen.tup3
        (fun1 A.observable B.generator)
        (fun1 C.observable A.generator)
        (R.generator D.generator C.generator)
    in
    let print (_, _, x) = print D.pp C.pp x in
    Util.test ~count ~print generator Laws.bifunctor_7 (fun lhs rhs (f, g, x) ->
        let f = Fn.apply f
        and g = Fn.apply g in
        let left = lhs f g x
        and right = rhs f g x in
        R.equal D.equal B.equal left right )
  ;;

  let tests ~count =
    [
      bifunctor_1 count
    ; bifunctor_2 count
    ; bifunctor_3 count
    ; bifunctor_4 count
    ; bifunctor_5 count
    ; bifunctor_6 count
    ; bifunctor_7 count
    ]
  ;;
end
