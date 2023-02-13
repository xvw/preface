open QCheck2

module Suite
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_FUNCTOR
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (Index : Model.T0) =
struct
  module Laws = Preface_laws.Indexed_functor.For (F)

  let print pp = Format.asprintf "%a" (R.pp pp Index.pp)

  let functor_1 count =
    let generator = R.generator A.generator Index.generator in
    let print = print A.pp in
    Util.test ~count ~print generator Laws.functor_1 (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal Index.equal left right )
  ;;

  let functor_2 count =
    let generator =
      let f = fun1 A.observable B.generator
      and g = fun1 C.observable A.generator in
      Gen.tup3 f g (R.generator C.generator Index.generator)
    in
    let print (_, _, x) = print C.pp x in
    Util.test ~count ~print generator Laws.functor_2 (fun lhs rhs (ff, gg, x) ->
        let f = Fn.apply ff
        and g = Fn.apply gg in
        let left = lhs f g x
        and right = rhs f g x in
        R.equal B.equal Index.equal left right )
  ;;

  let tests ~count = [ functor_1 count; functor_2 count ]
end
