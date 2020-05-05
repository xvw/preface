module Make
    (S : Preface_specs.SEMIGROUP)
    (R : Requirement.INPUT_T with type t = S.t) : Requirement.OUTPUT = struct
  open QCheck
  module Underlying = Preface_make.Semigroup.Via_combine (S)

  let combine_associative =
    let test_name = "combine is associative"
    and test_arbitrary = triple R.arbitrary R.arbitrary R.arbitrary
    and test (a, b, c) =
      let left = S.(combine (combine a b) c)
      and right = S.(combine a (combine b c)) in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let times =
    let test_name = "times"
    and test_arbitrary = pair (int_range 0 15) R.arbitrary
    and test (n, x) =
      let left = S.times n x
      and right = Underlying.times n x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let infix_combine =
    let test_name = "++"
    and test_arbitrary = pair R.arbitrary R.arbitrary
    and test (a, b) =
      let left = S.(a ++ b)
      and right = Underlying.(a ++ b) in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let reduce_nel =
    let test_name = "reduce_nel"
    and test_arbitrary = Arbitrary.nonempty_list R.arbitrary
    and test x =
      let left = S.reduce_nel x
      and right = Underlying.reduce_nel x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let cases =
    [
      ( "Semigroup " ^ R.name ^ " laws"
      , [ combine_associative ] |> List.map QCheck_alcotest.to_alcotest )
    ; ( "Semigroup " ^ R.name ^ " has expected behaviour"
      , [ times; infix_combine; reduce_nel ]
        |> List.map QCheck_alcotest.to_alcotest )
    ]
  ;;
end
