open QCheck2

module Suite
    (R : Model.COVARIANT_0)
    (S : Preface_specs.SEMIGROUP with type t = R.t) =
struct
  module Laws = Preface_laws.Semigroup.For (S)

  let print = Format.asprintf "%a" R.pp

  let semigroup_1 count =
    let print = Print.tup3 print print print in
    let generator = Gen.tup3 R.generator R.generator R.generator in
    Util.test ~count ~print generator Laws.semigroup_1 (fun lhs rhs (a, b, c) ->
        let left = lhs a b c
        and right = rhs a b c in
        R.equal left right )
  ;;

  let tests ~count = [ semigroup_1 count ]
end
