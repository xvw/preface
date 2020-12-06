open Preface_qcheck

module Combine_associative
    (F : Preface_specs.SEMIGROUP)
    (A : Model.T0 with type t = F.t) =
Preface_qcheck.Make.Test (struct
  let name = "combine is associative"

  type input = F.t * F.t * F.t

  type output = F.t

  let arbitrary = QCheck.triple A.arbitrary A.arbitrary A.arbitrary

  let equal = A.equal

  let left (a, b, c) = F.(combine (combine a b) c)

  let right (a, b, c) = F.(combine a (combine b c))
end)

module Cases (F : Preface_specs.SEMIGROUP) (A : Model.T0 with type t = F.t) =
struct
  module Combine_associative = Combine_associative (F) (A)

  let cases n =
    [ Combine_associative.test n ]
    |> Stdlib.List.map QCheck_alcotest.to_alcotest
  ;;
end
