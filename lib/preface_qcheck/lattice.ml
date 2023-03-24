open QCheck2

module Suite
    (R : Model.COVARIANT_0)
    (L : Preface_specs.LATTICE with type t = R.t) =
struct
  module Laws = Preface_laws.Lattice.For (L)

  let print = Format.asprintf "%a" R.pp

  module Join_semilattice = Join_semilattice.Suite (R) (L)
  module Meet_semilattice = Meet_semilattice.Suite (R) (L)

  let lattice_1 count =
    let print = Print.tup2 print print in
    let generator = Gen.tup2 R.generator R.generator in
    Util.test ~count ~print generator Laws.lattice_1 (fun lhs rhs (a, b) ->
        let left = lhs a b
        and right = rhs a b in
        R.equal left right )
  ;;

  let lattice_2 count =
    let print = Print.tup2 print print in
    let generator = Gen.tup2 R.generator R.generator in
    Util.test ~count ~print generator Laws.lattice_2 (fun lhs rhs (a, b) ->
        let left = lhs a b
        and right = rhs a b in
        R.equal left right )
  ;;

  let tests ~count =
    Join_semilattice.tests ~count
    @ Meet_semilattice.tests ~count
    @ [ lattice_1 count; lattice_2 count ]
  ;;
end
