module Make_hooked_behaviour
    (A : Preface_specs.ALTERNATIVE)
    (R : Requirement.INPUT_T1 with type 'a t = 'a A.t)
    (Hook : Requirement.HOOK with type 'a t = 'a A.t)
    (P : Sample.PACK) : Requirement.OUTPUT = struct
  open QCheck

  open Helper.Make_for_t1 (R) (P)

  module Applicative_test =
    Applicative.Make_hooked
      (A)
      (struct
        include R

        let name = "Alternative of " ^ R.name
      end)
      (Hook)
      (P)

  module Underlying = Preface_make.Alternative.Via_apply (A)

  let infix_combine =
    let test_name = "x <|> y = combine x y"
    and test_arbitrary = pair (over t1) (over t1)
    and test (x, y) =
      let left = A.Infix.(x <|> y)
      and right = Underlying.Infix.(x <|> y) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let cases =
    [
      ( "Alternative " ^ R.name ^ " has expected behaviour"
      , [ infix_combine ] |> List.map QCheck_alcotest.to_alcotest )
    ]
    @ Applicative_test.cases
  ;;
end

module Make_hooked_for_monoidal_behaviour
    (A : Preface_specs.ALTERNATIVE)
    (R : Requirement.INPUT_T1 with type 'a t = 'a A.t)
    (Hook : Requirement.HOOK with type 'a t = 'a A.t)
    (P : Sample.PACK) : Requirement.OUTPUT = struct
  open QCheck

  open Helper.Make_for_t1 (R) (P)

  module Behaviour_test = Make_hooked_behaviour (A) (R) (Hook) (P)

  let monoid_neutral_left =
    let test_name = "combine neutral x = x"
    and test_arbitrary = over t1
    and test x =
      let open A in
      let left = combine neutral x
      and right = x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let monoid_neutral_right =
    let test_name = "combine x neutral = x"
    and test_arbitrary = over t1
    and test x =
      let open A in
      let left = combine x neutral
      and right = x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let monoid_associative =
    let test_name = "combine u (combine v w) = combine (combine u v) w"
    and test_arbitrary = triple (over t1) (over t1) (over t1)
    and test (u, v, w) =
      let open A in
      let left = combine u (combine v w)
      and right = combine (combine u v) w in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let cases =
    [
      ( "Alternative " ^ R.name ^ " laws"
      , [ monoid_neutral_left; monoid_neutral_right; monoid_associative ]
        |> List.map QCheck_alcotest.to_alcotest )
    ]
    @ Behaviour_test.cases
  ;;
end

module Make_for_monoidal_behaviour
    (A : Preface_specs.ALTERNATIVE)
    (R : Requirement.INPUT_T1 with type 'a t = 'a A.t) =
  Make_hooked_for_monoidal_behaviour (A) (R)
    (struct
      type 'a t = 'a A.t

      let apply x = Obj.magic x
    end)

module Make_behaviour
    (A : Preface_specs.ALTERNATIVE)
    (R : Requirement.INPUT_T1 with type 'a t = 'a A.t) =
  Make_hooked_behaviour (A) (R)
    (struct
      type 'a t = 'a A.t

      let apply x = Obj.magic x
    end)
