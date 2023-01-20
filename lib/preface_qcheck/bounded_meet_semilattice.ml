module Suite
    (R : Model.COVARIANT_0)
    (L : Preface_specs.BOUNDED_MEET_SEMILATTICE with type t = R.t) =
struct
  module Laws = Preface_laws.Bounded_meet_semilattice.For (L)

  let print = Format.asprintf "%a" R.pp

  module Meet_semilattice = Meet_semilattice.Suite (R) (L)

  let bounded_meet_semilattice_1 count =
    let generator = R.generator in
    Util.test ~count ~print generator Laws.bounded_meet_semilattice_1
      (fun lhs rhs x ->
        let left = lhs x
        and right = rhs L.top in
        R.equal left right )
  ;;

  let tests ~count =
    Meet_semilattice.tests ~count @ [ bounded_meet_semilattice_1 count ]
  ;;
end
