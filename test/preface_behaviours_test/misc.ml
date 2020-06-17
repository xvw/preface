module Int_requirement (N : sig
  val name : string
end) =
struct
  type t = int

  let name = N.name

  let size = 1000

  let arbitrary = Preface_qcheck.Arbitrary.int
end

module Sum_semigroup = Preface_make.Semigroup.Via_combine (struct
  type t = int

  let combine = ( + )
end)

module Sum_monoid = Preface_make.Monoid.Via_combine_and_neutral (struct
  type t = int

  let combine = ( + )

  let neutral = 0
end)

module Prod_semigroup = Preface_make.Semigroup.Via_combine (struct
  type t = int

  let combine = ( * )
end)

module Prod_monoid = Preface_make.Monoid.Via_combine_and_neutral (struct
  type t = int

  let combine = ( * )

  let neutral = 1
end)

module Sum_semigroup_test =
  Preface_qcheck.Semigroup.Make
    (Sum_semigroup)
    (Int_requirement (struct
      let name = "Sum over int"
    end))

module Prod_semigroup_test =
  Preface_qcheck.Semigroup.Make
    (Prod_semigroup)
    (Int_requirement (struct
      let name = "Prod over int"
    end))

module Sum_monoid_test =
  Preface_qcheck.Monoid.Make
    (Sum_monoid)
    (Int_requirement (struct
      let name = "Sum over int"
    end))

module Prod_monoid_test =
  Preface_qcheck.Monoid.Make
    (Prod_monoid)
    (Int_requirement (struct
      let name = "Prod over int"
    end))

let cases =
  Sum_semigroup_test.cases
  @ Prod_semigroup_test.cases
  @ Sum_monoid_test.cases
  @ Prod_monoid_test.cases
;;
