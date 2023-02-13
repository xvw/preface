open QCheck2

module Suite
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_SELECTIVE
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (Index : Model.T0) =
struct
  module Laws = Preface_laws.Indexed_selective.For (F)
  module Applicative = Indexed_applicative.Suite (R) (F) (A) (B) (C) (Index)

  let print pp = Format.asprintf "%a" (R.pp pp Index.pp)

  let selective_1 count =
    let generator =
      R.generator (Util.gen_either A.generator A.generator) Index.generator
    in
    let print = print (Util.pp_either A.pp A.pp) in
    Util.test ~count ~print generator Laws.selective_1 (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal Index.equal left right )
  ;;

  let selective_2 count =
    let generator =
      let f = R.generator (fun1 A.observable B.generator) Index.generator in
      Gen.tup3 (Util.gen_either A.generator B.generator) f f
    in
    let print (e, _, _) = Format.asprintf "%a" (Util.pp_either A.pp B.pp) e in
    Util.test ~count ~print generator Laws.selective_2
      (fun lhs rhs (e, ff, gg) ->
        let f = F.(Fn.apply <$> ff)
        and g = F.(Fn.apply <$> gg) in
        let left = lhs e f g
        and right = rhs e f g in
        R.equal B.equal Index.equal left right )
  ;;

  let selective_3 count =
    let generator =
      let f =
        R.generator
          (Util.gen_either C.generator (fun1 A.observable B.generator))
          Index.generator
      and g =
        R.generator (fun2 C.observable A.observable B.generator) Index.generator
      in
      Gen.tup3
        (R.generator (Util.gen_either A.generator B.generator) Index.generator)
        f g
    in
    let print (e, _, _) = print (Util.pp_either A.pp B.pp) e in
    Util.test ~count ~print generator Laws.selective_3
      (fun lhs rhs (e, ff, gg) ->
        let f = F.(Stdlib.Either.map_right Fn.apply <$> ff)
        and g = F.(Fn.apply <$> gg) in
        let left = lhs e f g
        and right = rhs e f g in
        R.equal B.equal Index.equal left right )
  ;;

  let selective_4 count =
    let generator =
      Gen.tup3
        (fun1 A.observable B.generator)
        (R.generator (Util.gen_either C.generator A.generator) Index.generator)
        (R.generator (fun1 C.observable A.generator) Index.generator)
    in
    let print (_, e, _) = print (Util.pp_either C.pp A.pp) e in
    Util.test ~count ~print generator Laws.selective_4
      (fun lhs rhs (ff, e, gg) ->
        let f = Fn.apply ff
        and g = F.(Fn.apply <$> gg) in
        let left = lhs f e g
        and right = rhs f e g in
        R.equal B.equal Index.equal left right )
  ;;

  let selective_5 count =
    let generator =
      Gen.tup3
        (fun1 A.observable B.generator)
        (R.generator (Util.gen_either A.generator C.generator) Index.generator)
        (R.generator (fun1 B.observable C.generator) Index.generator)
    in
    let print (_, e, _) = print (Util.pp_either A.pp C.pp) e in
    Util.test ~count ~print generator Laws.selective_5
      (fun lhs rhs (ff, e, gg) ->
        let f = Fn.apply ff
        and g = F.(Fn.apply <$> gg) in
        let left = lhs f e g
        and right = rhs f e g in
        R.equal C.equal Index.equal left right )
  ;;

  let selective_6 count =
    let generator =
      Gen.tup3
        (fun2 A.observable B.observable C.generator)
        (R.generator (Util.gen_either B.generator C.generator) Index.generator)
        (R.generator A.generator Index.generator)
    in
    let print (_, x, y) =
      Print.tup2 (print @@ Util.pp_either B.pp C.pp) (print A.pp) (x, y)
    in
    Util.test ~count ~print generator Laws.selective_6
      (fun lhs rhs (ff, x, y) ->
        let f = Fn.apply ff in
        let left = lhs f x y
        and right = rhs f x y in
        R.equal C.equal Index.equal left right )
  ;;

  let selective_7 count =
    let generator =
      Gen.tup2
        (R.generator (Util.gen_either A.generator B.generator) Index.generator)
        (fun1 A.observable B.generator)
    in
    let print (x, _) = print (Util.pp_either A.pp B.pp) x in
    Util.test ~count ~print generator Laws.selective_7 (fun lhs rhs (x, ff) ->
        let f = Fn.apply ff in
        let left = lhs x f
        and right = rhs x f in
        R.equal B.equal Index.equal left right )
  ;;

  let tests ~count =
    Applicative.tests ~count
    @ [
        selective_1 count
      ; selective_2 count
      ; selective_3 count
      ; selective_4 count
      ; selective_5 count
      ; selective_6 count
      ; selective_7 count
      ]
  ;;
end

module Suite_rigid
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_SELECTIVE
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (Index : Model.T0) =
struct
  module Non_rigid = Suite (R) (F) (A) (B) (C) (Index)
  module Laws = Preface_laws.Indexed_selective.For_rigid (F)

  let print pp = Format.asprintf "%a" (R.pp pp Index.pp)

  let selective_8 count =
    let generator =
      Gen.tup2
        (R.generator (fun1 A.observable B.generator) Index.generator)
        (R.generator A.generator Index.generator)
    in
    let print (_, x) = print A.pp x in
    Util.test ~count ~print generator Laws.selective_8 (fun lhs rhs (ff, x) ->
        let f = F.(Fn.apply <$> ff) in
        let left = lhs f x
        and right = rhs f x in
        R.equal B.equal Index.equal left right )
  ;;

  let selective_9 count =
    let generator =
      Gen.tup3
        (R.generator A.generator Index.generator)
        (R.generator (Util.gen_either B.generator C.generator) Index.generator)
        (R.generator (fun1 B.observable C.generator) Index.generator)
    in
    let print (x, y, _) =
      Print.tup2 (print A.pp) (print (Util.pp_either B.pp C.pp)) (x, y)
    in
    Util.test ~count ~print generator Laws.selective_9
      (fun lhs rhs (x, y, ff) ->
        let f = F.(Fn.apply <$> ff) in
        let left = lhs x y f
        and right = rhs x y f in
        R.equal C.equal Index.equal left right )
  ;;

  let tests ~count =
    Non_rigid.tests ~count @ [ selective_8 count; selective_9 count ]
  ;;
end
