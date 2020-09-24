module Make_hooked_monoidal_laws
    (M : Preface_specs.MONAD_PLUS)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t)
    (Hook : Requirement.HOOK with type 'a t = 'a M.t)
    (P : Sample.PACK) : Requirement.OUTPUT = struct
  open QCheck

  open Helper.Make_for_t1 (R) (P)

  let left_identity =
    let test_name = "neutral <|> x = x"
    and test_arbitrary = over t1
    and test x =
      let open M in
      let left = neutral <|> x
      and right = x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let right_identity =
    let test_name = "x <|> neutral = x"
    and test_arbitrary = over t1
    and test x =
      let open M in
      let left = x <|> neutral
      and right = x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let associativity =
    let test_name = "(a <|> b) <|> c = a <|> (b <|> c)"
    and test_arbitrary = triple (over t1) (over t1) (over t1)
    and test (a, b, c) =
      let open M in
      let left = a <|> b <|> c
      and right = a <|> (b <|> c) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let cases =
    [
      ( "Monad plus " ^ R.name ^ " has a monoidal behaviour"
      , [ left_identity; right_identity; associativity ]
        |> List.map QCheck_alcotest.to_alcotest )
    ]
  ;;
end

module Make_hooked_left_zero_law
    (M : Preface_specs.MONAD_PLUS)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t)
    (Hook : Requirement.HOOK with type 'a t = 'a M.t)
    (P : Sample.PACK) : Requirement.OUTPUT = struct
  open QCheck

  open Helper.Make_for_t1 (R) (P)

  let left_zero =
    let test_name = "neutral >>= x = neutral"
    and test_arbitrary = fun1 t1' (over t2)
    and test f' =
      let open M in
      let f = Fn.apply f' in
      let left = neutral >>= f
      and right = neutral in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let cases =
    [
      ( "Monad plus " ^ R.name ^ " has a left zero behaviour"
      , [ left_zero ] |> List.map QCheck_alcotest.to_alcotest )
    ]
  ;;
end

module Make_hooked_left_distribution_law
    (M : Preface_specs.MONAD_PLUS)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t)
    (Hook : Requirement.HOOK with type 'a t = 'a M.t)
    (P : Sample.PACK) : Requirement.OUTPUT = struct
  open QCheck

  open Helper.Make_for_t1 (R) (P)

  let left_distribution =
    let test_name = "(a <|> b) >>= f = (a >>= f) <|> (b >>= f)"
    and test_arbitrary = triple (over t1) (over t1) (fun1 t1' (over t2))
    and test (a, b, f') =
      let open M in
      let f = Fn.apply f' in
      let left = a <|> b >>= f
      and right = a >>= f <|> (b >>= f) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let cases =
    [
      ( "Monad plus " ^ R.name ^ " has a left distribution behaviour"
      , [ left_distribution ] |> List.map QCheck_alcotest.to_alcotest )
    ]
  ;;
end

module Make_hooked_left_catch_law
    (M : Preface_specs.MONAD_PLUS)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t)
    (Hook : Requirement.HOOK with type 'a t = 'a M.t)
    (P : Sample.PACK) : Requirement.OUTPUT = struct
  open QCheck

  open Helper.Make_for_t1 (R) (P)

  let left_catch =
    let test_name = "(return a) <|> b = return a"
    and test_arbitrary = pair t1 (over t1)
    and test (a, b) =
      let open M in
      let left = return a <|> b
      and right = return a in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let cases =
    [
      ( "Monad plus " ^ R.name ^ " has a left catch behaviour"
      , [ left_catch ] |> List.map QCheck_alcotest.to_alcotest )
    ]
  ;;
end

module Make_hooked_behaviour
    (M : Preface_specs.MONAD_PLUS)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t)
    (Hook : Requirement.HOOK with type 'a t = 'a M.t)
    (P : Sample.PACK) : Requirement.OUTPUT = struct
  open QCheck

  open Helper.Make_for_t1 (R) (P)

  module Monad_test =
    Monad.Make_hooked
      (M)
      (struct
        include R

        let name = "Monad_plus of " ^ R.name
      end)
      (Hook)
      (P)

  module Underlying = Preface_make.Monad_plus.Via_bind (M)

  let infix_combine =
    let test_name = "x <|> y = combine x y"
    and test_arbitrary = pair (over t1) (over t1)
    and test (x, y) =
      let left = M.Infix.(x <|> y)
      and right = Underlying.Infix.(x <|> y) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let filter =
    let test_name = "filter f x"
    and test_arbitrary = pair (over t1) (fun1 t1' bool)
    and test (x, f') =
      let f = Fn.apply f' in
      let left = M.filter f x
      and right = Underlying.filter f x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let cases =
    [
      ( "Monad plus " ^ R.name ^ " has expected behaviour"
      , [ infix_combine; filter ] |> List.map QCheck_alcotest.to_alcotest )
    ]
    @ Monad_test.cases
  ;;
end

module Make_behaviour
    (M : Preface_specs.MONAD_PLUS)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t) =
  Make_hooked_behaviour (M) (R)
    (struct
      type 'a t = 'a M.t

      let apply x = Obj.magic x
    end)

module Make_monoidal_laws
    (M : Preface_specs.MONAD_PLUS)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t) =
  Make_hooked_monoidal_laws (M) (R)
    (struct
      type 'a t = 'a M.t

      let apply x = Obj.magic x
    end)

module Make_left_zero_law
    (M : Preface_specs.MONAD_PLUS)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t) =
  Make_hooked_left_zero_law (M) (R)
    (struct
      type 'a t = 'a M.t

      let apply x = Obj.magic x
    end)

module Make_left_distribution_law
    (M : Preface_specs.MONAD_PLUS)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t) =
  Make_hooked_left_distribution_law (M) (R)
    (struct
      type 'a t = 'a M.t

      let apply x = Obj.magic x
    end)

module Make_left_catch_law
    (M : Preface_specs.MONAD_PLUS)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t) =
  Make_hooked_left_catch_law (M) (R)
    (struct
      type 'a t = 'a M.t

      let apply x = Obj.magic x
    end)
