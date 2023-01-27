module Sum = Preface.Make.Monoid.Via_combine_and_neutral (struct
  type t = int

  let neutral = 0
  let combine = Int.add
end)

module Prod = Preface.Make.Monoid.Via_combine_and_neutral (struct
  type t = int

  let neutral = 1
  let combine = Int.mul
end)

module Ord = struct
  type t =
    | Lt
    | Eq
    | Gt

  let pp ppf = function
    | Lt -> Format.fprintf ppf "Lt"
    | Eq -> Format.fprintf ppf "Eq"
    | Gt -> Format.fprintf ppf "Gt"
  ;;

  let equal a b =
    match (a, b) with
    | Eq, Eq -> true
    | Lt, Lt -> true
    | Gt, Gt -> true
    | _ -> false
  ;;

  let generator =
    let open QCheck2.Gen in
    frequency [ (3, pure Lt); (3, pure Eq); (3, pure Gt) ]
  ;;

  let observable =
    let hash = function
      | Lt -> Hashtbl.seeded_hash 1 Lt
      | Eq -> Hashtbl.seeded_hash 2 Eq
      | Gt -> Hashtbl.seeded_hash 3 Gt
    and print = Format.asprintf "%a" pp
    and eq = equal in
    QCheck2.Observable.make ~hash ~eq print
  ;;

  module Meet_semilattice = Preface.Make.Meet_semilattice.Via_meet (struct
    type nonrec t = t

    let meet x y =
      match (x, y) with
      | Lt, _ | _, Lt -> Lt
      | Gt, x | x, Gt -> x
      | Eq, Eq -> Eq
    ;;
  end)

  module Join_semilattice = Preface.Make.Join_semilattice.Via_join (struct
    type nonrec t = t

    let join x y =
      match (x, y) with
      | Gt, _ | _, Gt -> Gt
      | Lt, x | x, Lt -> x
      | Eq, Eq -> Eq
    ;;
  end)
end

module Bool_meet_semilattice = Preface.Make.Meet_semilattice.Via_meet (struct
  type t = bool

  let meet x y = x && y
end)

module Bool_join_semilattice = Preface.Make.Join_semilattice.Via_join (struct
  type t = bool

  let join x y = x || y
end)

module Bool_bounded_meet_semilattice =
Preface.Make.Bounded_meet_semilattice.Via_meet_and_top (struct
  include Bool_meet_semilattice

  let top = false
end)

module Bool_bounded_join_semilattice =
Preface.Make.Bounded_join_semilattice.Via_join_and_bottom (struct
  include Bool_join_semilattice

  let bottom = true
end)

module Sum_monoid_suite = Preface.Qcheck.Monoid.Suite (Sample.Int) (Sum)
module Prod_monoid_suite = Preface.Qcheck.Monoid.Suite (Sample.Int) (Prod)

module Bool_meet_semilattice_suite =
  Preface.Qcheck.Meet_semilattice.Suite (Sample.Bool) (Bool_meet_semilattice)

module Ordering_meet_semilattice_suite =
  Preface.Qcheck.Meet_semilattice.Suite (Ord) (Ord.Meet_semilattice)

module Bool_join_semilattice_suite =
  Preface.Qcheck.Join_semilattice.Suite (Sample.Bool) (Bool_join_semilattice)

module Bool_bounded_meet_semilattice_suite =
  Preface.Qcheck.Bounded_meet_semilattice.Suite
    (Sample.Bool)
    (Bool_bounded_meet_semilattice)

module Bool_bounded_join_semilattice_suite =
  Preface.Qcheck.Bounded_join_semilattice.Suite
    (Sample.Bool)
    (Bool_bounded_join_semilattice)

module Ordering_join_semilattice_suite =
  Preface.Qcheck.Join_semilattice.Suite (Ord) (Ord.Join_semilattice)

module YOCaml_profunctor =
  Preface.Qcheck.Profunctor.Suite
    (Req.Mini_yocaml.Req)
    (Req.Mini_yocaml.Profunctor)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)
    (Sample.Bool)
    (Sample.Float)

module YOCaml_strong =
  Preface.Qcheck.Strong.Suite (Req.Mini_yocaml.Req) (Req.Mini_yocaml.Strong)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)
    (Sample.Bool)
    (Sample.Float)

module YOCaml_choice =
  Preface.Qcheck.Choice.Suite (Req.Mini_yocaml.Req) (Req.Mini_yocaml.Choice)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)
    (Sample.Bool)
    (Sample.Float)

module YOCaml_semigroupoid =
  Preface.Qcheck.Semigroupoid.Suite
    (Req.Mini_yocaml.Req)
    (Req.Mini_yocaml.Category)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)

module YOCaml_category =
  Preface.Qcheck.Category.Suite (Req.Mini_yocaml.Req) (Req.Mini_yocaml.Category)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)

module YOCaml_arrow =
  Preface.Qcheck.Arrow.Suite (Req.Mini_yocaml.Req) (Req.Mini_yocaml.Arrow)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)

module YOCaml_arrow_choice =
  Preface.Qcheck.Arrow_choice.Suite
    (Req.Mini_yocaml.Req)
    (Req.Mini_yocaml.Arrow_choice)
    (Sample.Int)
    (Sample.String)
    (Sample.Float)
    (Sample.Char)

let cases ~count =
  Util.with_alcotest ~count
    [
      ("Sum monoid", Sum_monoid_suite.tests)
    ; ("Prod Monoid", Sum_monoid_suite.tests)
    ; ("Bool Meet_semilattice", Bool_meet_semilattice_suite.tests)
    ; ("Ord Meet_semilattice", Ordering_meet_semilattice_suite.tests)
    ; ("Bool Join_semilattice", Bool_join_semilattice_suite.tests)
    ; ( "Bool Bounded_meet_semilattice"
      , Bool_bounded_meet_semilattice_suite.tests )
    ; ( "Bool Bounded_join_semilattice"
      , Bool_bounded_join_semilattice_suite.tests )
    ; ("Ord Join_semilattice", Ordering_join_semilattice_suite.tests)
    ; ("YOCaml Profunctor", YOCaml_profunctor.tests)
    ; ("YOCaml Strong", YOCaml_strong.tests)
    ; ("YOCaml Choice", YOCaml_choice.tests)
    ; ("YOCaml Semigroupoid", YOCaml_semigroupoid.tests)
    ; ("YOCaml Categrory", YOCaml_category.tests)
    ; ("YOCaml Arrow", YOCaml_arrow.tests)
    ; ("YOCaml Arrow Choice", YOCaml_arrow_choice.tests)
    ]
;;
