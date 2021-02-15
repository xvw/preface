open Preface_qcheck

module Left_identity (F : Preface_specs.MONOID) (A : Model.T0 with type t = F.t) =
Preface_qcheck.Make.Test (struct
  let name = "neutral <|> x = x"

  type input = F.t

  type output = F.t

  let arbitrary = A.arbitrary

  let equal = A.equal

  let left x = F.(neutral <|> x)

  let right x = x
end)

module Right_identity
    (F : Preface_specs.MONOID)
    (A : Model.T0 with type t = F.t) =
Preface_qcheck.Make.Test (struct
  let name = "x <|> neutral = x"

  type input = F.t

  type output = F.t

  let arbitrary = A.arbitrary

  let equal = A.equal

  let left x = F.(x <|> neutral)

  let right x = x
end)

module Cases (F : Preface_specs.MONOID) (A : Model.T0 with type t = F.t) =
struct
  module Semigroup = Semigroup.Cases (F) (A)
  module Left = Left_identity (F) (A)
  module Right = Right_identity (F) (A)

  let cases n =
    Semigroup.cases n
    @ ( [ Left.test n; Right.test n ]
      |> Stdlib.List.map QCheck_alcotest.to_alcotest )
  ;;
end
