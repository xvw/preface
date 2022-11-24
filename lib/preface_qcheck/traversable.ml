open QCheck2

module Suite_applicative
    (R : Model.COVARIANT_1)
    (T : Preface_specs.Traversable.API_OVER_APPLICATIVE with type 'a t = 'a R.t)
    (RF : Model.COVARIANT_1)
    (F : Preface_specs.APPLICATIVE with type 'a t = 'a RF.t)
    (RG : Model.COVARIANT_1)
    (G : Preface_specs.APPLICATIVE with type 'a t = 'a RG.t) (NT : sig
      val run : 'a F.t -> 'a G.t
    end)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) =
struct
  module Laws = Preface_laws.Traversable.For_applicative (T)
  module Compose = Laws.Compose (F) (G)
  module Naturality = Laws.Naturality (F) (G) (NT)

  let print pp = Format.asprintf "%a" (R.pp pp)

  let traversable_1 count =
    let generator = R.generator A.generator in
    let print = print A.pp in
    Util.test ~count ~print generator Laws.traversable_1 (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal left right )
  ;;

  let travserable_2 count =
    let generator =
      Gen.tup3
        (fun1 A.observable (RF.generator B.generator))
        (fun1 B.observable (RG.generator C.generator))
        (R.generator A.generator)
    in
    let print (_, _, x) = print A.pp x in
    Util.test ~count ~print generator Compose.traversable_composition_1
      (fun lhs rhs (ff, gg, x) ->
        let f = Fn.apply ff
        and g = Fn.apply gg in
        let left = lhs f g x
        and right = rhs f g x in
        RF.equal (RG.equal (R.equal C.equal)) left right )
  ;;

  let travserable_3 count =
    let generator =
      Gen.tup2
        (fun1 A.observable (RF.generator B.generator))
        (R.generator A.generator)
    in
    let print (_, x) = print A.pp x in
    Util.test ~count ~print generator Naturality.traversable_naturality_1
      (fun lhs rhs (ff, x) ->
        let f = Fn.apply ff in
        let left = lhs f x
        and right = rhs f x in
        RG.equal (R.equal B.equal) left right )
  ;;

  let tests ~count =
    [ traversable_1 count; travserable_2 count; travserable_3 count ]
  ;;
end

module Suite_monad
    (R : Model.COVARIANT_1)
    (T : Preface_specs.Traversable.API_OVER_MONAD with type 'a t = 'a R.t)
    (A : Model.T0) =
struct
  module Laws = Preface_laws.Traversable.For_monad (T)

  let print pp = Format.asprintf "%a" (R.pp pp)

  let traversable_1 count =
    let generator = R.generator A.generator in
    let print = print A.pp in
    Util.test ~count ~print generator Laws.traversable_1 (fun lhs rhs x ->
        let left = lhs x
        and right = rhs x in
        R.equal A.equal left right )
  ;;

  let tests ~count = [ traversable_1 count ]
end
