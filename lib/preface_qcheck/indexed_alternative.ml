open QCheck2

module Suite_monoidal_aux
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_ALTERNATIVE
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (Index : Model.T0) =
struct
  module Laws = Preface_laws.Indexed_alternative.For_monoidal (F)

  let print pp = Format.asprintf "%a" (R.pp pp Index.pp)

  let alternative_monoid_1 count =
    let generator = R.generator A.generator Index.generator in
    let print = print A.pp in
    Util.test ~count ~print generator Laws.alternative_monoid_1
      (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal Index.equal left right )
  ;;

  let alternative_monoid_2 count =
    let generator = R.generator A.generator Index.generator in
    let print = print A.pp in
    Util.test ~count ~print generator Laws.alternative_monoid_2
      (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal Index.equal left right )
  ;;

  let alternative_monoid_3 count =
    let generator =
      Gen.tup3
        (R.generator A.generator Index.generator)
        (R.generator A.generator Index.generator)
        (R.generator A.generator Index.generator)
    in
    let print = Print.tup3 (print A.pp) (print A.pp) (print A.pp) in
    Util.test ~count ~print generator Laws.alternative_monoid_3
      (fun lhs rhs (x, y, z) ->
        let left = lhs x y z
        and right = rhs x y z in
        R.equal A.equal Index.equal left right )
  ;;

  let tests ~count =
    [
      alternative_monoid_1 count
    ; alternative_monoid_2 count
    ; alternative_monoid_3 count
    ]
  ;;
end

module Suite_right_distributivity_aux
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_ALTERNATIVE
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (Index : Model.T0) =
struct
  module Laws = Preface_laws.Indexed_alternative.For_right_distributivity (F)

  let print pp = Format.asprintf "%a" (R.pp pp Index.pp)

  let alternative_right_distrib_1 count =
    let generator =
      let f = R.generator (fun1 A.observable B.generator) Index.generator in
      Gen.tup3 f f (R.generator A.generator Index.generator)
    in

    let print (_, _, x) = print A.pp x in
    Util.test ~count ~print generator Laws.alternative_right_distrib_1
      (fun lhs rhs (ff, gg, x) ->
        let f = F.(Fn.apply <$> ff)
        and g = F.(Fn.apply <$> gg) in
        let left = lhs f g x
        and right = rhs f g x in
        R.equal B.equal Index.equal left right )
  ;;

  let tests ~count = [ alternative_right_distrib_1 count ]
end

module Suite_right_absorbtion_aux
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_ALTERNATIVE
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (Index : Model.T0) =
struct
  module Laws = Preface_laws.Indexed_alternative.For_right_absorbtion (F)

  let print pp = Format.asprintf "%a" (R.pp pp Index.pp)

  let alternative_right_absorb_1 count =
    let generator = R.generator A.generator Index.generator in
    let print = print A.pp in
    Util.test ~count ~print generator Laws.alternative_right_absorb_1
      (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal Index.equal left right )
  ;;

  let tests ~count = [ alternative_right_absorb_1 count ]
end

module Suite_monoidal
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_ALTERNATIVE
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (Index : Model.T0) =
struct
  module Applicative = Indexed_applicative.Suite (R) (F) (A) (B) (C) (Index)
  module Alternative = Suite_monoidal_aux (R) (F) (A) (B) (Index)

  let tests ~count = Applicative.tests ~count @ Alternative.tests ~count
end

module Suite_right_distributivity
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_ALTERNATIVE
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (Index : Model.T0) =
struct
  module Applicative = Indexed_applicative.Suite (R) (F) (A) (B) (C) (Index)
  module Alternative = Suite_right_distributivity_aux (R) (F) (A) (B) (Index)

  let tests ~count = Applicative.tests ~count @ Alternative.tests ~count
end

module Suite_right_absorbtion
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_ALTERNATIVE
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (Index : Model.T0) =
struct
  module Applicative = Indexed_applicative.Suite (R) (F) (A) (B) (C) (Index)
  module Alternative = Suite_right_absorbtion_aux (R) (F) (A) (B) (Index)

  let tests ~count = Applicative.tests ~count @ Alternative.tests ~count
end

module Suite
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_ALTERNATIVE
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (Index : Model.T0) =
struct
  module Applicative = Indexed_applicative.Suite (R) (F) (A) (B) (C) (Index)
  module Monoid = Suite_monoidal_aux (R) (F) (A) (B) (Index)
  module Distrib = Suite_right_distributivity_aux (R) (F) (A) (B) (Index)
  module Absorb = Suite_right_absorbtion_aux (R) (F) (A) (B) (Index)

  let tests ~count =
    Applicative.tests ~count
    @ Monoid.tests ~count
    @ Distrib.tests ~count
    @ Absorb.tests ~count
  ;;
end
