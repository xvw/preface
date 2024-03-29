open QCheck2

module Suite
    (R : Model.COVARIANT_1)
    (I : Preface_specs.INVARIANT with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) =
struct
  module Laws = Preface_laws.Invariant.For (I)

  let print pp' = Format.asprintf "%a" (R.pp pp')

  let invariant_1 count =
    let print = print A.pp in
    let generator = R.generator A.generator in
    Util.test ~count ~print generator Laws.invariant_1 (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal left right )
  ;;

  let invariant_2 count =
    let print (_, x) = Format.asprintf "%a" (R.pp C.pp) x in
    let generator =
      let g = fun1 A.observable B.generator
      and g' = fun1 B.observable A.generator
      and f = fun1 C.observable A.generator
      and f' = fun1 A.observable C.generator in
      Gen.tup2 (Gen.tup4 g g' f f') (R.generator C.generator)
    in
    Util.test ~count ~print generator Laws.invariant_2
      (fun lhs rhs ((fg, fg', ff, ff'), x) ->
        let g = Fn.apply fg
        and g' = Fn.apply fg'
        and f = Fn.apply ff
        and f' = Fn.apply ff' in
        let left = lhs g g' f f' x
        and right = rhs g g' f f' x in
        R.equal B.equal left right )
  ;;

  let tests ~count = [ invariant_1 count; invariant_2 count ]
end

module Suite_contravariant
    (R : Model.CONTRAVARIANT_1)
    (I : Preface_specs.INVARIANT with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) =
struct
  module Laws = Preface_laws.Invariant.For (I)

  let invariant_1 count =
    let generator = R.generator A.observable
    and input = R.input A.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.invariant_1
      (fun lhs rhs (c, x) ->
        let c = R.lift c in
        let left = lhs c
        and right = rhs c in
        R.run_equality x left right )
  ;;

  let invariant_2 count =
    let generator =
      Gen.tup5
        (fun1 A.observable B.generator)
        (fun1 B.observable A.generator)
        (fun1 C.observable A.generator)
        (fun1 A.observable C.generator)
        (R.generator C.observable)
    and input = R.input B.generator in
    Util.test ~count (Gen.tup2 generator input) Laws.invariant_2
      (fun lhs rhs ((f, g, h, i, c), x) ->
        let f = Fn.apply f
        and g = Fn.apply g
        and h = Fn.apply h
        and i = Fn.apply i
        and c = R.lift c in
        let left = lhs f g h i c
        and right = rhs f g h i c in
        R.run_equality x left right )
  ;;

  let tests ~count = [ invariant_1 count; invariant_2 count ]
end
