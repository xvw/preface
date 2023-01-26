open QCheck2

module Suite
    (R : Model.COVARIANT_0)
    (L : Preface_specs.JOIN_SEMILATTICE with type t = R.t) =
struct
  module Laws = Preface_laws.Join_semilattice.For (L)

  let print = Format.asprintf "%a" R.pp

  let join_semilattice_1 count =
    let print = Print.tup3 print print print in
    let generator = Gen.tup3 R.generator R.generator R.generator in
    Util.test ~count ~print generator Laws.join_semilattice_1
      (fun lhs rhs (x, y, z) ->
        let left = lhs x y z
        and right = rhs x y z in
        R.equal left right )
  ;;

  let join_semilattice_2 count =
    let print = Print.tup2 print print in
    let generator = Gen.tup2 R.generator R.generator in
    Util.test ~count ~print generator Laws.join_semilattice_2
      (fun lhs rhs (x, y) ->
        let left = lhs x y
        and right = rhs x y in
        R.equal left right )
  ;;

  let join_semilattice_3 count =
    let generator = R.generator in
    Util.test ~count ~print generator Laws.join_semilattice_3 (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal left right )
  ;;

  let tests ~count =
    [
      join_semilattice_1 count
    ; join_semilattice_2 count
    ; join_semilattice_3 count
    ]
  ;;
end
