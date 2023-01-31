open QCheck2

module Suite_monoidal_aux
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_MONAD_PLUS
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (Index : Model.T0) =
struct
  module Laws = Preface_laws.Indexed_monad_plus.For_monoidal (F)

  let print pp = Format.asprintf "%a" (R.pp pp Index.pp)

  let monad_plus_monoid_1 count =
    let generator = R.generator A.generator Index.generator in
    let print = print A.pp in
    Util.test ~count ~print generator Laws.monad_plus_monoid_1 (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal Index.equal left right )
  ;;

  let monad_plus_monoid_2 count =
    let generator = R.generator A.generator Index.generator in
    let print = print A.pp in
    Util.test ~count ~print generator Laws.monad_plus_monoid_2 (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal Index.equal left right )
  ;;

  let monad_plus_monoid_3 count =
    let generator =
      Gen.tup3
        (R.generator A.generator Index.generator)
        (R.generator A.generator Index.generator)
        (R.generator A.generator Index.generator)
    in
    let print = Print.tup3 (print A.pp) (print A.pp) (print A.pp) in
    Util.test ~count ~print generator Laws.monad_plus_monoid_3
      (fun lhs rhs (x, y, z) ->
        let left = lhs x y z
        and right = rhs x y z in
        R.equal A.equal Index.equal left right )
  ;;

  let tests ~count =
    [
      monad_plus_monoid_1 count
    ; monad_plus_monoid_2 count
    ; monad_plus_monoid_3 count
    ]
  ;;
end

module Suite_left_absorption_aux
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_MONAD_PLUS
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (Index : Model.T0) =
struct
  module Laws = Preface_laws.Indexed_monad_plus.For_left_absorption (F)

  let monad_plus_left_absorb_1 count =
    let generator =
      fun1 A.observable (R.generator B.generator Index.generator)
    in
    Util.test ~count generator Laws.monad_plus_left_absorb_1 (fun lhs rhs ff ->
        let f = Fn.apply ff in
        let left = lhs f
        and right = rhs f in
        R.equal B.equal Index.equal left right )
  ;;

  let tests ~count = [ monad_plus_left_absorb_1 count ]
end

module Suite_left_distributivity_aux
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_MONAD_PLUS
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (Index : Model.T0) =
struct
  module Laws = Preface_laws.Indexed_monad_plus.For_left_distributivity (F)

  let print pp = Format.asprintf "%a" (R.pp pp Index.pp)

  let monad_plus_left_distrib_1 count =
    let generator =
      Gen.tup3
        (R.generator A.generator Index.generator)
        (R.generator A.generator Index.generator)
        (fun1 A.observable (R.generator B.generator Index.generator))
    in
    let print (x, y, _) = Print.tup2 (print A.pp) (print A.pp) (x, y) in
    Util.test ~count ~print generator Laws.monad_plus_left_distrib_1
      (fun lhs rhs (x, y, ff) ->
        let f = Fn.apply ff in
        let left = lhs x y f
        and right = rhs x y f in
        R.equal B.equal Index.equal left right )
  ;;

  let tests ~count = [ monad_plus_left_distrib_1 count ]
end

module Suite_left_catch_aux
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_MONAD_PLUS
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (Index : Model.T0) =
struct
  module Laws = Preface_laws.Indexed_monad_plus.For_left_catch (F)

  let print pp = Format.asprintf "%a" (R.pp pp Index.pp)

  let monad_plus_left_catch count =
    let generator =
      Gen.tup2 A.generator (R.generator A.generator Index.generator)
    in
    let print = Print.tup2 (Format.asprintf "%a" A.pp) (print A.pp) in
    Util.test ~count ~print generator Laws.monad_plus_left_catch_1
      (fun lhs rhs (x, m) ->
        let left = lhs x m
        and right = rhs x m in
        R.equal A.equal Index.equal left right )
  ;;

  let tests ~count = [ monad_plus_left_catch count ]
end

module Suite_monoidal
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_MONAD_PLUS
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0)
    (Index : Model.T0) =
struct
  module Monad = Indexed_monad.Suite (R) (F) (A) (B) (C) (D) (Index)
  module Monad_plus = Suite_monoidal_aux (R) (F) (A) (B) (Index)

  let tests ~count = Monad.tests ~count @ Monad_plus.tests ~count
end

module Suite_left_absorption
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_MONAD_PLUS
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0)
    (Index : Model.T0) =
struct
  module Monad = Indexed_monad.Suite (R) (F) (A) (B) (C) (D) (Index)
  module Monad_plus = Suite_left_absorption_aux (R) (F) (A) (B) (Index)

  let tests ~count = Monad.tests ~count @ Monad_plus.tests ~count
end

module Suite_left_distributivity
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_MONAD_PLUS
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0)
    (Index : Model.T0) =
struct
  module Monad = Indexed_monad.Suite (R) (F) (A) (B) (C) (D) (Index)
  module Monad_plus = Suite_left_distributivity_aux (R) (F) (A) (B) (Index)

  let tests ~count = Monad.tests ~count @ Monad_plus.tests ~count
end

module Suite_left_catch
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_MONAD_PLUS
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0)
    (Index : Model.T0) =
struct
  module Monad = Indexed_monad.Suite (R) (F) (A) (B) (C) (D) (Index)
  module Monad_plus = Suite_left_catch_aux (R) (F) (A) (Index)

  let tests ~count = Monad.tests ~count @ Monad_plus.tests ~count
end

module Suite
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_MONAD_PLUS
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0)
    (Index : Model.T0) =
struct
  module Monad = Indexed_monad.Suite (R) (F) (A) (B) (C) (D) (Index)
  module Monoid = Suite_monoidal_aux (R) (F) (A) (B) (Index)
  module Absorb = Suite_left_absorption_aux (R) (F) (A) (B) (Index)
  module Distrib = Suite_left_distributivity_aux (R) (F) (A) (B) (Index)
  module Catch = Suite_left_catch_aux (R) (F) (A) (Index)

  let tests ~count =
    Monad.tests ~count
    @ Monoid.tests ~count
    @ Absorb.tests ~count
    @ Distrib.tests ~count
    @ Catch.tests ~count
  ;;
end
