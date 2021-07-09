module Int_req = struct
  type t = int

  let arbitrary = QCheck.int
  let observable = QCheck.Observable.int
  let equal = Int.equal
end

module String_req = struct
  type t = string

  let arbitrary = QCheck.string
  let observable = QCheck.Observable.string
  let equal = String.equal
end

module Sum_semigroup = Preface_make.Semigroup.Via_combine (struct
  type t = int

  let combine = ( + )
end)

module Sum_monoid =
  Preface_make.Monoid.Over_semigroup
    (Sum_semigroup)
    (struct
      type t = int

      let neutral = 0
    end)

module Prod_semigroup = Preface_make.Semigroup.Via_combine (struct
  type t = int

  let combine = ( * )
end)

module Prod_monoid =
  Preface_make.Monoid.Over_semigroup
    (Prod_semigroup)
    (struct
      type t = int

      let neutral = 1
    end)

module String_semigroup = Preface_make.Semigroup.Via_combine (struct
  type t = string

  let combine = ( ^ )
end)

module String_monoid =
  Preface_make.Monoid.Over_semigroup
    (String_semigroup)
    (struct
      type t = string

      let neutral = ""
    end)

module Sum_semigroup_cases =
  Preface_laws_pbt.Semigroup.Cases (Sum_semigroup) (Int_req)

module Prod_semigroup_cases =
  Preface_laws_pbt.Semigroup.Cases (Prod_semigroup) (Int_req)

module String_semigroup_cases =
  Preface_laws_pbt.Semigroup.Cases (String_semigroup) (String_req)

module Sum_monoid_cases = Preface_laws_pbt.Monoid.Cases (Sum_monoid) (Int_req)
module Prod_monoid_cases = Preface_laws_pbt.Monoid.Cases (Prod_monoid) (Int_req)

module String_monoid_cases =
  Preface_laws_pbt.Monoid.Cases (String_monoid) (String_req)

let cases n =
  [
    ("Int with sum Semigroup Laws", Sum_semigroup_cases.cases n)
  ; ("Int with prod Semigroup Laws", Prod_semigroup_cases.cases n)
  ; ("String Semigroup Laws", String_semigroup_cases.cases n)
  ; ("Int with sum Monoid Laws", Sum_monoid_cases.cases n)
  ; ("Int with prod SemigroupMonoid Laws", Prod_monoid_cases.cases n)
  ; ("String Monoid Laws", String_monoid_cases.cases n)
  ]
;;
