open QCheck2

module Suite
    (R : Model.COVARIANT_0)
    (L : Preface_specs.BOUNDED_LATTICE with type t = R.t) =
struct
  module Laws = Preface_laws.Bounded_lattice.For (L)

  let print = Format.asprintf "%a" R.pp

  module Bounded_join_semilattice = Bounded_join_semilattice.Suite (R) (L)
  module Bounded_meet_semilattice = Bounded_meet_semilattice.Suite (R) (L)

  let bounded_lattice_1 count =
    let print = Print.tup2 print print in
    let generator = Gen.tup2 R.generator R.generator in
    Util.test ~count ~print generator Laws.bounded_lattice_1
      (fun lhs rhs (a, b) ->
        let left = lhs a b
        and right = rhs a b in
        R.equal left right )
  ;;

  let bounded_lattice_2 count =
    let print = Print.tup2 print print in
    let generator = Gen.tup2 R.generator R.generator in
    Util.test ~count ~print generator Laws.bounded_lattice_1
      (fun lhs rhs (a, b) ->
        let left = lhs a b
        and right = rhs a b in
        R.equal left right )
  ;;

  let tests ~count =
    Bounded_join_semilattice.tests ~count
    @ Bounded_meet_semilattice.tests ~count
    @ [ bounded_lattice_1 count; bounded_lattice_2 count ]
  ;;
end
