module type FUNCTOR = Preface_specs.FUNCTOR

module Make_with_post_hook
    (Functor : FUNCTOR)
    (Req : Qcheck_helpers.REQ with type 'a t = 'a Functor.t)
    (Hook : Qcheck_helpers.HOOK with type 'a t = 'a Functor.t)
    (X : Qcheck_helpers.GENERATOR)
    (F : Qcheck_helpers.GENERATOR)
    (G : Qcheck_helpers.GENERATOR) : Qcheck_helpers.ALCOTEST_SUITE = struct
  open QCheck

  (* Combinators for QCheck generators *)
  let x = X.arbitrary

  let f = F.arbitrary

  let g = G.arbitrary

  let functor_of = Req.arbitrary

  let x_input = X.observable

  let g_input = G.observable

  (* Test *)

  let preserve_identity =
    let test_name = "map id = id"
    and test_arbitrary = functor_of x
    and test x =
      let open Functor in
      let left = map (fun i -> i) x
      and right = x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let preserve_morphisms =
    let test_name = "map (f % g) = map f % map g"
    and test_arbitrary = triple (functor_of x) (fun1 g_input f) (fun1 x_input g)
    and test (x, f', g') =
      let open Functor in
      let f = Fn.apply f'
      and g = Fn.apply g' in
      let left = map (fun i -> f (g i)) x
      and right = (map f) (map g x) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let replace =
    let test_name = "replace = map % const"
    and test_arbitrary = pair (functor_of x) x
    and test (x, value) =
      let open Functor in
      let open Preface_core.Fun in
      let left = replace value x
      and right = (map % const) value x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let void =
    let test_name = "void = replace ()"
    and test_arbitrary = functor_of x
    and test x =
      let open Functor in
      let left = void x
      and right = replace () x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let infix_map =
    let test_name = "<$> = map"
    and test_arbitrary = pair (fun1 x_input f) (functor_of x)
    and test (f', x) =
      let open Functor in
      let f = Fn.apply f' in
      let left = f <$> x
      and right = map f x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let infix_rmap =
    let test_name = "<&> = flip map"
    and test_arbitrary = pair (fun1 x_input f) (functor_of x)
    and test (f', x) =
      let open Functor in
      let f = Fn.apply f' in
      let left = x <&> f
      and right = map f x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let infix_replace =
    let test_name = "<$ = replace"
    and test_arbitrary = pair (functor_of x) f
    and test (x, value) =
      let open Functor in
      let open Preface_core.Fun in
      let left = value <$ x
      and right = (Functor.map % const) value x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let infix_rreplace =
    let test_name = "$> = flip replace"
    and test_arbitrary = pair (functor_of x) f
    and test (x, value) =
      let open Functor in
      let open Preface_core.Fun in
      let left = x $> value
      and right = (map % const) value x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let cases =
    ( Req.suite_name ^ " functor"
    , List.map QCheck_alcotest.to_alcotest
        [
          preserve_identity
        ; preserve_morphisms
        ; replace
        ; void
        ; infix_map
        ; infix_rmap
        ; infix_replace
        ; infix_rreplace
        ] )
  ;;
end

module Make
    (Functor : FUNCTOR)
    (Req : Qcheck_helpers.REQ with type 'a t = 'a Functor.t) =
  Make_with_post_hook (Functor) (Req)
    (struct
      type 'a t = 'a Functor.t

      let apply x = Obj.magic x
    end)
