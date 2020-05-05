module Make_hooked
    (F : Preface_specs.FUNCTOR)
    (R : Requirement.INPUT_T1 with type 'a t = 'a F.t)
    (Hook : Requirement.HOOK with type 'a t = 'a F.t)
    (P : Sample.PACK) : Requirement.OUTPUT = struct
  open QCheck

  open Helper.Make_for_t1 (R) (P)

  module Underlying = Preface_make.Functor.Via_map (F)

  (* Test Functor laws *)

  let preserve_identity =
    let test_name = "map id = id"
    and test_arbitrary = over t1
    and test x =
      let open F in
      let left = map (fun i -> i) x
      and right = x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let preserve_morphisms =
    let test_name = "map (f % g) = map f % map g"
    and test_arbitrary = triple (over t1) (fun1 t2' t3) (fun1 t1' t2)
    and test (x, f', g') =
      let open F in
      let f = Fn.apply f'
      and g = Fn.apply g' in
      let left = map (fun i -> f (g i)) x
      and right = (map f) (map g x) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  (* Test underlying functor *)

  let replace =
    let test_name = "replace"
    and test_arbitrary = pair t1 (over t2)
    and test (x, y) =
      let left = F.(replace x y)
      and right = Underlying.(replace x y) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let void =
    let test_name = "void"
    and test_arbitrary = over t1
    and test x =
      let left = F.(void x)
      and right = Underlying.(void x) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let infix_map =
    let test_name = "<$>"
    and test_arbitrary = pair (over t1) (fun1 t1' t2)
    and test (x, f') =
      let f = Fn.apply f' in
      let left = F.(f <$> x)
      and right = Underlying.(f <$> x) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let infixr_map =
    let test_name = "<&>"
    and test_arbitrary = pair (over t1) (fun1 t1' t2)
    and test (x, f') =
      let f = Fn.apply f' in
      let left = F.(x <&> f)
      and right = Underlying.(x <&> f) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let infix_replace =
    let test_name = "<$"
    and test_arbitrary = pair t1 (over t2)
    and test (x, y) =
      let left = F.(x <$ y)
      and right = Underlying.(x <$ y) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let infixr_replace =
    let test_name = "$>"
    and test_arbitrary = pair t1 (over t2)
    and test (x, y) =
      let left = F.(y $> x)
      and right = Underlying.(y $> x) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let cases =
    [
      ( "Functor " ^ R.name ^ " laws"
      , [ preserve_identity; preserve_morphisms ]
        |> List.map QCheck_alcotest.to_alcotest )
    ; ( "Functor " ^ R.name ^ " has expected behaviour"
      , [ replace; void; infix_map; infixr_map; infix_replace; infixr_replace ]
        |> List.map QCheck_alcotest.to_alcotest )
    ]
  ;;
end

module Make
    (F : Preface_specs.FUNCTOR)
    (R : Requirement.INPUT_T1 with type 'a t = 'a F.t) =
  Make_hooked (F) (R)
    (struct
      type 'a t = 'a F.t

      let apply x = Obj.magic x
    end)
