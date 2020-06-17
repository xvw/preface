module Make
    (M : Preface_specs.MONOID)
    (R : Requirement.INPUT_T with type t = M.t) : Requirement.OUTPUT = struct
  open QCheck
  module Underlying = Preface_make.Monoid.Via_combine_and_neutral (M)

  module Semigroup_test =
    Semigroup.Make
      (M)
      (struct
        include R

        let name = "Monoid of " ^ R.name
      end)

  let left_identity =
    let test_name = "neutral ++ x = x"
    and test_arbitrary = R.arbitrary
    and test x =
      let left = M.combine M.neutral x
      and right = x in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let right_identity =
    let test_name = "x ++ neutral = x"
    and test_arbitrary = R.arbitrary
    and test x =
      let left = M.combine x M.neutral
      and right = x in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let reduce =
    let test_name = "reduce"
    and test_arbitrary = Arbitrary.small_list R.arbitrary
    and test x =
      let left = M.reduce x
      and right = Underlying.reduce x in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let cases =
    [
      ( "Monoid " ^ R.name ^ " laws"
      , [ left_identity; right_identity ]
        |> List.map QCheck_alcotest.to_alcotest )
    ; ( "Monoid " ^ R.name ^ " has expected behaviour"
      , [ reduce ] |> List.map QCheck_alcotest.to_alcotest )
    ]
    @ Semigroup_test.cases
  ;;
end
