module Make_hooked
    (M : Preface_specs.MONAD)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t)
    (Hook : Requirement.HOOK with type 'a t = 'a M.t)
    (P : Sample.PACK) : Requirement.OUTPUT = struct
  open QCheck

  open Helper.Make_for_t1 (R) (P)

  module Underlying = Preface_make.Monad.Via_bind (M)

  (* Test Monad laws *)

  let join_map_1 =
    let test_name = "join % join = join % map join"
    and test_arbitrary = over (over (over t1))
    and test x =
      let open M in
      let open Preface_core.Fun in
      let left = (join % join) x
      and right = (join % map join) x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let join_map_2 =
    let test_name = "join % map return = join % return = id"
    and test_arbitrary = over t1
    and test x =
      let open M in
      let open Preface_core.Fun in
      let left = (join % map return) x
      and center = (join % return) x
      and right = id x in
      Hook.(apply left = apply center && apply center = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let natural_transformation_1 =
    let test_name = "map id = id"
    and test_arbitrary = over t1
    and test x =
      let open M in
      let open Preface_core.Fun in
      let left = map id x
      and right = id x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let natural_transformation_2 =
    let test_name = "map (f % g) = map f % map g"
    and test_arbitrary = triple (over t1) (fun1 t3' t2) (fun1 t1' t3)
    and test (x, f', g') =
      let open M in
      let open Preface_core.Fun in
      let f = Fn.apply f'
      and g = Fn.apply g' in
      let left = map (f % g) x
      and right = (map f % map g) x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let natural_transformation_3 =
    let test_name = "map f % join = join % map (map f)"
    and test_arbitrary = pair (over (over t1)) (fun1 t1' t2)
    and test (x, f') =
      let open M in
      let open Preface_core.Fun in
      let f = Fn.apply f' in
      let left = (map f % join) x
      and right = (join % map (map f)) x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let natural_transformation_4 =
    let test_name = "map f % return = return f"
    and test_arbitrary = pair t1 (fun1 t1' t2)
    and test (x, f') =
      let open M in
      let open Preface_core.Fun in
      let f = Fn.apply f' in
      let left = (map f % return) x
      and right = (return % f) x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let left_identity =
    let test_name = "return x >>= f = f x"
    and test_arbitrary = pair t1 (fun1 t1' (over t2))
    and test (x, f') =
      let open M in
      let f = Fn.apply f' in
      let left = return x >>= f
      and right = f x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let right_identity =
    let test_name = "x >>= return = x"
    and test_arbitrary = over t1
    and test x =
      let open M in
      let left = x >>= return
      and right = x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let associativity =
    let test_name = "(x >>= f) >>= g = x >>= (fun y -> f y >>= g)"
    and test_arbitrary =
      triple (over t1) (fun1 t1' (over t2)) (fun1 t2' (over t3))
    and test (x, f', g') =
      let open M in
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let left = x >>= f >>= g
      and right = x >>= (fun y -> f y >>= g) in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let kleisli_left_identity =
    let test_name = "return >=> f = f"
    and test_arbitrary = pair t1 (fun1 t1' (over t2))
    and test (x, f') =
      let open M in
      let f = Fn.apply f' in
      let left = (return >=> f) x
      and right = f x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let kleisli_right_identity =
    let test_name = "f >=> return = f"
    and test_arbitrary = pair t1 (fun1 t1' (over t2))
    and test (x, f') =
      let open M in
      let f = Fn.apply f' in
      let left = (f >=> return) x
      and right = f x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let kleisli_associativity =
    let test_name = "(f >=> g) >=> h = f >=> (g >=> h)"
    and test_arbitrary =
      quad t1 (fun1 t1' (over t2)) (fun1 t2' (over t3)) (fun1 t3' (over t4))
    and test (x, f', g', h') =
      let open M in
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let h = Fn.apply h' in
      let left = (f >=> g >=> h) x
      and right = (f >=> (g >=> h)) x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  (* Test underlying Monad *)

  let map =
    let test_name = "map"
    and test_arbitrary = pair (over t1) (fun1 t1' t2)
    and test (x, f') =
      let f = Fn.apply f' in
      let left = M.(map f x)
      and right = Underlying.(map f x) in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let join =
    let test_name = "join"
    and test_arbitrary = over (over t1)
    and test x =
      let left = M.(join x)
      and right = Underlying.(join x) in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let bind =
    let test_name = "bind"
    and test_arbitrary = pair (over t1) (fun1 t1' (over t2))
    and test (x, f') =
      let f = Fn.apply f' in
      let left = M.(bind f x)
      and right = Underlying.(bind f x) in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let compose_left_to_right =
    let test_name = "compose_left_to_right"
    and test_arbitrary = triple t1 (fun1 t1' (over t2)) (fun1 t2' (over t3))
    and test (x, f', g') =
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let left = M.(compose_left_to_right f g x)
      and right = Underlying.(compose_left_to_right f g x) in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let void =
    let test_name = "void"
    and test_arbitrary = over t1
    and test x =
      let left = M.(void x)
      and right = Underlying.(void x) in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let compose_right_to_left =
    let test_name = "compose_right_to_left"
    and test_arbitrary = triple t1 (fun1 t2' (over t3)) (fun1 t1' (over t2))
    and test (x, f', g') =
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let left = M.(compose_right_to_left f g x)
      and right = Underlying.(compose_right_to_left f g x) in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let lift =
    let test_name = "lift"
    and test_arbitrary = pair (over t1) (fun1 t1' t2)
    and test (x, f') =
      let f = Fn.apply f' in
      let left = M.(lift f x)
      and right = Underlying.(lift f x) in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let lift2 =
    let test_name = "lift2"
    and test_arbitrary = triple (over t1) (over t2) (fun2 t1' t2' t3)
    and test (a, b, f') =
      let f = Fn.apply f' in
      let left = M.lift2 f a b
      and right = Underlying.lift2 f a b in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let lift3 =
    let test_name = "lift3"
    and test_arbitrary =
      quad (over t1) (over t2) (over t3) (fun3 t1' t2' t3' t3)
    and test (a, b, c, f') =
      let f = Fn.apply f' in
      let left = M.lift3 f a b c
      and right = Underlying.lift3 f a b c in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let syntax_let_bind =
    let test_name = "let*"
    and test_arbitrary = pair (over t1) (fun1 t1' (over t2))
    and test (x, f') =
      let f = Fn.apply f' in
      let left =
        let open M in
        let* a = x in
        f a
      and right =
        let open Underlying in
        let* a = x in
        f a
      in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let syntax_let_map =
    let test_name = "let+"
    and test_arbitrary = pair (over t1) (fun1 t1' t2)
    and test (x, f') =
      let f = Fn.apply f' in
      let left =
        let open M in
        let+ a = x in
        f a
      and right =
        let open Underlying in
        let+ a = x in
        f a
      in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let infix_map =
    let test_name = ">|="
    and test_arbitrary = pair (over t1) (fun1 t1' t2)
    and test (x, f') =
      let f = Fn.apply f' in
      let left = M.(x >|= f)
      and right = Underlying.(x >|= f) in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let infixr_map =
    let test_name = "=|<"
    and test_arbitrary = pair (over t1) (fun1 t1' t2)
    and test (x, f') =
      let f = Fn.apply f' in
      let left = M.(f =|< x)
      and right = Underlying.(f =|< x) in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let infix_bind =
    let test_name = ">>="
    and test_arbitrary = pair (over t1) (fun1 t1' (over t2))
    and test (x, f') =
      let f = Fn.apply f' in
      let left = M.(x >>= f)
      and right = Underlying.(x >>= f) in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let infixr_bind =
    let test_name = "=<<"
    and test_arbitrary = pair (over t1) (fun1 t1' (over t2))
    and test (x, f') =
      let f = Fn.apply f' in
      let left = M.(f =<< x)
      and right = Underlying.(f =<< x) in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let infix_kleisli =
    let test_name = ">=>"
    and test_arbitrary = triple t1 (fun1 t1' (over t2)) (fun1 t2' (over t3))
    and test (x, f', g') =
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let left = M.((f >=> g) x)
      and right = Underlying.((f >=> g) x) in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let infixr_kleisli =
    let test_name = "<=<"
    and test_arbitrary = triple t1 (fun1 t2' (over t3)) (fun1 t1' (over t2))
    and test (x, f', g') =
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let left = M.((f <=< g) x)
      and right = Underlying.((f <=< g) x) in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let infix_seq =
    let test_name = ">>"
    and test_arbitrary = over t1
    and test x =
      let left = M.(return () >> x)
      and right = Underlying.(return () >> x) in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let infixr_seq =
    let test_name = "<<"
    and test_arbitrary = over t1
    and test x =
      let left = M.(x << return ())
      and right = Underlying.(x << return ()) in
      Hook.(apply left = apply right)
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let cases =
    [
      ( "Monad " ^ R.name ^ " laws"
      , [
          join_map_1
        ; join_map_2
        ; natural_transformation_1
        ; natural_transformation_2
        ; natural_transformation_3
        ; natural_transformation_4
        ; left_identity
        ; right_identity
        ; associativity
        ; kleisli_left_identity
        ; kleisli_right_identity
        ; kleisli_associativity
        ]
        |> List.map QCheck_alcotest.to_alcotest )
    ; ( "Monad " ^ R.name ^ " has expected behaviour"
      , [
          map
        ; join
        ; bind
        ; compose_left_to_right
        ; void
        ; compose_right_to_left
        ; lift
        ; lift2
        ; lift3
        ; syntax_let_bind
        ; syntax_let_map
        ; infix_map
        ; infixr_map
        ; infix_bind
        ; infixr_bind
        ; infix_kleisli
        ; infixr_kleisli
        ; infix_seq
        ; infixr_seq
        ]
        |> List.map QCheck_alcotest.to_alcotest )
    ]
  ;;
end

module Make
    (M : Preface_specs.MONAD)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t) =
  Make_hooked (M) (R)
    (struct
      type 'a t = 'a M.t

      let apply x = Obj.magic x
    end)
