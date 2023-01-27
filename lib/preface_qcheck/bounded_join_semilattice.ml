module Suite
    (R : Model.COVARIANT_0)
    (L : Preface_specs.BOUNDED_JOIN_SEMILATTICE with type t = R.t) =
struct
  module Laws = Preface_laws.Bounded_join_semilattice.For (L)

  let print = Format.asprintf "%a" R.pp

  module Join_semilattice = Join_semilattice.Suite (R) (L)

  let bounded_meet_semilattice_1 count =
    let generator = R.generator in
    Util.test ~count ~print generator Laws.bounded_join_semilattice_1
      (fun lhs rhs x ->
        let left = lhs x
        and right = rhs L.bottom in
        R.equal left right )
  ;;

  let tests ~count =
    Join_semilattice.tests ~count @ [ bounded_meet_semilattice_1 count ]
  ;;
end
