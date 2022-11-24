open QCheck2

module Suite
    (R : Model.COVARIANT_1)
    (F : Preface_specs.ALT with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) =
struct
  module Laws = Preface_laws.Alt.For (F)
  module Functor = Functor.Suite (R) (F) (A) (B) (C)

  let print pp = Format.asprintf "%a" (R.pp pp)

  let alt_1 count =
    let generator =
      Gen.tup3 (R.generator A.generator) (R.generator A.generator)
        (R.generator A.generator)
    in
    let print = Print.tup3 (print A.pp) (print A.pp) (print A.pp) in
    Util.test ~count ~print generator Laws.alt_1 (fun lhs rhs (a, b, c) ->
        let left = lhs a b c
        and right = rhs a b c in
        R.equal A.equal left right )
  ;;

  let alt_2 count =
    let generator =
      Gen.tup3
        (fun1 A.observable B.generator)
        (R.generator A.generator) (R.generator A.generator)
    in
    let print (_, x, y) = Print.tup2 (print A.pp) (print A.pp) (x, y) in
    Util.test ~count ~print generator Laws.alt_2 (fun lhs rhs (ff, b, c) ->
        let f = Fn.apply ff in
        let left = lhs f b c
        and right = rhs f b c in
        R.equal B.equal left right )
  ;;

  let tests ~count = Functor.tests ~count @ [ alt_1 count; alt_2 count ]
end
