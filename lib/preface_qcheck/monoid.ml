module Suite
    (R : Model.COVARIANT_0)
    (M : Preface_specs.MONOID with type t = R.t) =
struct
  module Laws = Preface_laws.Monoid.For (M)
  module Semigroup_laws = Semigroup.Suite (R) (M)

  let print = Format.asprintf "%a" R.pp

  let monoid_1 count =
    Util.test ~count ~print R.generator Laws.monoid_1 (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal left right )
  ;;

  let monoid_2 count =
    Util.test ~count ~print R.generator Laws.monoid_2 (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal left right )
  ;;

  let tests ~count =
    Semigroup_laws.tests ~count @ [ monoid_1 count; monoid_2 count ]
  ;;
end
