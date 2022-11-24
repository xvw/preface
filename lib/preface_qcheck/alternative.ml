open QCheck2

module Suite_monoidal_aux
    (R : Model.COVARIANT_1)
    (F : Preface_specs.ALTERNATIVE with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0) =
struct
  module Laws = Preface_laws.Alternative.For_monoidal (F)

  let print pp = Format.asprintf "%a" (R.pp pp)

  let alternative_monoid_1 count =
    let generator = R.generator A.generator in
    let print = print A.pp in
    Util.test ~count ~print generator Laws.alternative_monoid_1
      (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal left right )
  ;;

  let alternative_monoid_2 count =
    let generator = R.generator A.generator in
    let print = print A.pp in
    Util.test ~count ~print generator Laws.alternative_monoid_2
      (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal left right )
  ;;

  let alternative_monoid_3 count =
    let generator =
      Gen.tup3 (R.generator A.generator) (R.generator A.generator)
        (R.generator A.generator)
    in
    let print = Print.tup3 (print A.pp) (print A.pp) (print A.pp) in
    Util.test ~count ~print generator Laws.alternative_monoid_3
      (fun lhs rhs (x, y, z) ->
        let left = lhs x y z
        and right = rhs x y z in
        R.equal A.equal left right )
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
    (R : Model.COVARIANT_1)
    (F : Preface_specs.ALTERNATIVE with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0) =
struct
  module Laws = Preface_laws.Alternative.For_right_distributivity (F)

  let print pp = Format.asprintf "%a" (R.pp pp)

  let alternative_right_distrib_1 count =
    let generator =
      let f = R.generator (fun1 A.observable B.generator) in
      Gen.tup3 f f (R.generator A.generator)
    in

    let print (_, _, x) = print A.pp x in
    Util.test ~count ~print generator Laws.alternative_right_distrib_1
      (fun lhs rhs (ff, gg, x) ->
        let f = F.(Fn.apply <$> ff)
        and g = F.(Fn.apply <$> gg) in
        let left = lhs f g x
        and right = rhs f g x in
        R.equal B.equal left right )
  ;;

  let tests ~count = [ alternative_right_distrib_1 count ]
end

module Suite_right_absorbtion_aux
    (R : Model.COVARIANT_1)
    (F : Preface_specs.ALTERNATIVE with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0) =
struct
  module Laws = Preface_laws.Alternative.For_right_absorbtion (F)

  let print pp = Format.asprintf "%a" (R.pp pp)

  let alternative_right_absorb_1 count =
    let generator = R.generator A.generator in
    let print = print A.pp in
    Util.test ~count ~print generator Laws.alternative_right_absorb_1
      (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal left right )
  ;;

  let tests ~count = [ alternative_right_absorb_1 count ]
end

module Suite_monoidal
    (R : Model.COVARIANT_1)
    (F : Preface_specs.ALTERNATIVE with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) =
struct
  module Applicative = Applicative.Suite (R) (F) (A) (B) (C)
  module Alternative = Suite_monoidal_aux (R) (F) (A) (B)

  let tests ~count = Applicative.tests ~count @ Alternative.tests ~count
end

module Suite_right_distributivity
    (R : Model.COVARIANT_1)
    (F : Preface_specs.ALTERNATIVE with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) =
struct
  module Applicative = Applicative.Suite (R) (F) (A) (B) (C)
  module Alternative = Suite_right_distributivity_aux (R) (F) (A) (B)

  let tests ~count = Applicative.tests ~count @ Alternative.tests ~count
end

module Suite_right_absorbtion
    (R : Model.COVARIANT_1)
    (F : Preface_specs.ALTERNATIVE with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) =
struct
  module Applicative = Applicative.Suite (R) (F) (A) (B) (C)
  module Alternative = Suite_right_absorbtion_aux (R) (F) (A) (B)

  let tests ~count = Applicative.tests ~count @ Alternative.tests ~count
end

module Suite
    (R : Model.COVARIANT_1)
    (F : Preface_specs.ALTERNATIVE with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) =
struct
  module Applicative = Applicative.Suite (R) (F) (A) (B) (C)
  module Monoid = Suite_monoidal_aux (R) (F) (A) (B)
  module Distrib = Suite_right_distributivity_aux (R) (F) (A) (B)
  module Absorb = Suite_right_absorbtion_aux (R) (F) (A) (B)

  let tests ~count =
    Applicative.tests ~count
    @ Monoid.tests ~count
    @ Distrib.tests ~count
    @ Absorb.tests ~count
  ;;
end
