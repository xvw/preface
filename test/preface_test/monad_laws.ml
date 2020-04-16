module type MONAD = Preface_specs.MONAD

module Make_with_post_hook
    (Monad : MONAD)
    (Req : Qcheck_helpers.REQ with type 'a t = 'a Monad.t)
    (Hook : Qcheck_helpers.HOOK with type 'a t = 'a Monad.t)
    (X : Qcheck_helpers.GENERATOR)
    (F : Qcheck_helpers.GENERATOR)
    (G : Qcheck_helpers.GENERATOR)
    (H : Qcheck_helpers.GENERATOR) : Qcheck_helpers.ALCOTEST_SUITE = struct
  open QCheck

  (* Combinators for QCheck generator *)
  let monad_of = Req.arbitrary

  let x = X.arbitrary

  let f = F.arbitrary

  let g = G.arbitrary

  let h = H.arbitrary

  let x_input = X.observable

  let f_input = F.observable

  let g_input = G.observable

  let join_map_1 =
    let test_name = "join % join = join % map join"
    and test_arbitrary = monad_of (monad_of (monad_of x))
    and test x =
      let open Monad in
      let open Preface_core.Fun in
      let left = (join % join) x
      and right = (join % map join) x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let join_map_2 =
    let test_name = "join % map return = join % return = id"
    and test_arbitrary = monad_of x
    and test x =
      let open Monad in
      let open Preface_core.Fun in
      let left = (join % map return) x
      and center = (join % return) x
      and right = id x in
      Hook.(apply left = apply center && apply center = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let natural_transformation_1 =
    let test_name = "map id = id"
    and test_arbitrary = monad_of x
    and test x =
      let open Monad in
      let open Preface_core.Fun in
      let left = map id x
      and right = id x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let natural_transformation_2 =
    let test_name = "map (f % g) = map f % map g"
    and test_arbitrary = triple (monad_of x) (fun1 g_input f) (fun1 x_input g)
    and test (x, f', g') =
      let open Monad in
      let open Preface_core.Fun in
      let f = Fn.apply f'
      and g = Fn.apply g' in
      let left = map (f % g) x
      and right = (map f % map g) x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let natural_transformation_3 =
    let test_name = "map f % join = join % map (map f)"
    and test_arbitrary = pair (monad_of (monad_of x)) (fun1 x_input f)
    and test (x, f') =
      let open Monad in
      let open Preface_core.Fun in
      let f = Fn.apply f' in
      let left = (map f % join) x
      and right = (join % map (map f)) x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let natural_transformation_4 =
    let test_name = "map f % return = return f"
    and test_arbitrary = pair x (fun1 x_input f)
    and test (x, f') =
      let open Monad in
      let open Preface_core.Fun in
      let f = Fn.apply f' in
      let left = (map f % return) x
      and right = (return % f) x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let left_identity =
    let test_name = "return x >>= f = f x"
    and test_arbitrary = pair x (fun1 x_input (monad_of f))
    and test (x, f') =
      let open Monad in
      let f = Fn.apply f' in
      let left = return x >>= f
      and right = f x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let right_identity =
    let test_name = "x >>= return = x"
    and test_arbitrary = monad_of x
    and test x =
      let open Monad in
      let left = x >>= return
      and right = x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let associativity =
    let test_name = "(x >>= f) >>= g = x >>= (fun y -> f y >>= g)"
    and test_arbitrary =
      triple (monad_of x)
        (fun1 x_input (monad_of f))
        (fun1 f_input (monad_of g))
    and test (x, f', g') =
      let open Monad in
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let left = x >>= f >>= g
      and right = x >>= (fun y -> f y >>= g) in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let kleisli_left_identity =
    let test_name = "return >=> f = f"
    and test_arbitrary = pair x (fun1 x_input (monad_of f))
    and test (x, f') =
      let open Monad in
      let f = Fn.apply f' in
      let left = (return >=> f) x
      and right = f x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let kleisli_right_identity =
    let test_name = "f >=> return = f"
    and test_arbitrary = pair x (fun1 x_input (monad_of f))
    and test (x, f') =
      let open Monad in
      let f = Fn.apply f' in
      let left = (f >=> return) x
      and right = f x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let kleisli_associativity =
    let test_name = "(f >=> g) >=> h = f >=> (g >=> h)"
    and test_arbitrary =
      quad x
        (fun1 x_input (monad_of f))
        (fun1 f_input (monad_of g))
        (fun1 g_input (monad_of h))
    and test (x, f', g', h') =
      let open Monad in
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let h = Fn.apply h' in
      let left = (f >=> g >=> h) x
      and right = (f >=> (g >=> h)) x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let join =
    let test_name = "join = bind id"
    and test_arbitrary = monad_of (monad_of x)
    and test x =
      let open Monad in
      let open Preface_core.Fun in
      let left = join x
      and right = bind id x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let map =
    let test_name = "map f = bind (return % f)"
    and test_arbitrary = pair (monad_of x) (fun1 x_input f)
    and test (x, f') =
      let open Monad in
      let open Preface_core.Fun in
      let f = Fn.apply f' in
      let left = map f x
      and right = bind (return % f) x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let compose_left_to_right =
    let test_name = "compose_left_to_right f g x = bind g (f x)"
    and test_arbitrary =
      triple x (fun1 x_input (monad_of f)) (fun1 f_input (monad_of g))
    and test (x, f', g') =
      let open Monad in
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let left = compose_left_to_right f g x
      and right = bind g (f x) in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let compose_right_to_left =
    let test_name = "compose_right_to_left f g x = compose_left_to_right g f x"
    and test_arbitrary =
      triple x (fun1 g_input (monad_of f)) (fun1 x_input (monad_of g))
    and test (x, f', g') =
      let open Monad in
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let left = compose_right_to_left f g x
      and right = compose_left_to_right g f x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let syntax_map =
    let test_name = "let+ x = m in f x = map f m"
    and test_arbitrary = pair (monad_of x) (fun1 x_input f)
    and test (x, f') =
      let open Monad in
      let f = Fn.apply f' in
      let left = map f x
      and right =
        let+ a = x in
        f a
      in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let infix_map =
    let test_name = "f =|< x = map f x"
    and test_arbitrary = pair (monad_of x) (fun1 x_input f)
    and test (x, f') =
      let open Monad in
      let f = Fn.apply f' in
      let left = map f x
      and right = f =|< x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let infix_rmap =
    let test_name = "x >|= f = map"
    and test_arbitrary = pair (monad_of x) (fun1 x_input f)
    and test (x, f') =
      let open Monad in
      let f = Fn.apply f' in
      let left = map f x
      and right = x >|= f in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let infix_compose_left_to_right =
    let test_name = "(>=>) = compose_left_to_right"
    and test_arbitrary =
      triple x (fun1 x_input (monad_of f)) (fun1 f_input (monad_of g))
    and test (x, f', g') =
      let open Monad in
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let left = compose_left_to_right f g x
      and right = (f >=> g) x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let infix_compose_right_to_left =
    let test_name = "(<=<) = compose_right_to_left"
    and test_arbitrary =
      triple x (fun1 g_input (monad_of f)) (fun1 x_input (monad_of g))
    and test (x, f', g') =
      let open Monad in
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let left = compose_right_to_left f g x
      and right = (f <=< g) x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let void =
    let test_name = "void x = return ()"
    and test_arbitrary = monad_of x
    and test x =
      let open Monad in
      let left = void x
      and right = return () in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let lift =
    let test_name = "lift = map"
    and test_arbitrary = pair (monad_of x) (fun1 x_input f)
    and test (x, f') =
      let open Monad in
      let f = Fn.apply f' in
      let left = map f x
      and right = lift f x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let lift2 =
    let test_name = "lift2 f a = ma >>= fun x -> mb >>= fun y -> return (f x y)"
    and test_arbitrary =
      triple (monad_of x) (monad_of f) (fun2 x_input f_input g)
    and test (a, b, f') =
      let open Monad in
      let f = Fn.apply f' in
      let left = a >>= (fun x -> b >>= (fun y -> return (f x y)))
      and right = lift2 f a b in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let lift3 =
    let test_name =
      "lift3 f a = ma >>= fun x -> mb >>= fun y -> mc >>= fun z -> return (f x \
       y z)"
    and test_arbitrary =
      quad (monad_of x) (monad_of f) (monad_of g)
        (fun3 x_input f_input g_input h)
    and test (a, b, c, f') =
      let open Monad in
      let f = Fn.apply f' in
      let left =
        a >>= (fun x -> b >>= (fun y -> c >>= (fun z -> return (f x y z))))
      and right = lift3 f a b c in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let syntax_bind =
    let test_name = "let* x = m in f x = m >>= f"
    and test_arbitrary = pair (monad_of x) (fun1 x_input (monad_of f))
    and test (x, f') =
      let open Monad in
      let f = Fn.apply f' in
      let left = x >>= f
      and right =
        let* a = x in
        f a
      in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let infix_bind =
    let test_name = "x >>= f = bind f x"
    and test_arbitrary = pair (monad_of x) (fun1 x_input (monad_of f))
    and test (x, f') =
      let open Monad in
      let f = Fn.apply f' in
      let left = x >>= f
      and right = bind f x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let infix_rbind =
    let test_name = "f =<< x = bind f x"
    and test_arbitrary = pair (monad_of x) (fun1 x_input (monad_of f))
    and test (x, f') =
      let open Monad in
      let f = Fn.apply f' in
      let left = f =<< x
      and right = bind f x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let infix_seq_compose =
    let test_name = "m >> n = m >>= (fun _ -> n)"
    and test_arbitrary = monad_of x
    and test x =
      let open Monad in
      let left = return () >> x
      and right = return () >>= (fun _ -> x) in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let infix_rseq_compose =
    let test_name = "m << n = m"
    and test_arbitrary = monad_of x
    and test x =
      let open Monad in
      let left = x << return ()
      and right = x in
      Hook.(apply left = apply right)
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let cases =
    ( Req.suite_name ^ " monad"
    , List.map QCheck_alcotest.to_alcotest
        [
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
        ; join
        ; map
        ; compose_left_to_right
        ; compose_right_to_left
        ; syntax_map
        ; infix_map
        ; infix_rmap
        ; infix_compose_left_to_right
        ; infix_compose_right_to_left
        ; void
        ; lift
        ; lift2
        ; lift3
        ; infix_bind
        ; infix_rbind
        ; syntax_bind
        ; infix_seq_compose
        ; infix_rseq_compose
        ] )
  ;;
end

module Make
    (Monad : MONAD)
    (Req : Qcheck_helpers.REQ with type 'a t = 'a Monad.t) =
  Make_with_post_hook (Monad) (Req)
    (struct
      type 'a t = 'a Monad.t

      let apply x = Obj.magic x
    end)
