module type APPLICATIVE = Preface_specs.APPLICATIVE

module Make_with_post_hook
    (Applicative : APPLICATIVE)
    (Req : Qcheck_helpers.REQ with type 'a t = 'a Applicative.t)
    (Hook : Qcheck_helpers.HOOK with type 'a t = 'a Applicative.t)
    (X : Qcheck_helpers.GENERATOR)
    (F : Qcheck_helpers.GENERATOR)
    (U : Qcheck_helpers.GENERATOR)
    (V : Qcheck_helpers.GENERATOR)
    (W : Qcheck_helpers.GENERATOR) : Qcheck_helpers.ALCOTEST_SUITE = struct
  open QCheck

  (* Combinators for QCheck generator *)
  let x = X.arbitrary

  let f = F.arbitrary

  let u = U.arbitrary

  let v = V.arbitrary

  let w = W.arbitrary

  let applicative_of = Req.arbitrary

  let x_input = X.observable

  let f_input = F.observable

  let u_input = U.observable

  let v_input = V.observable

  let w_input = W.observable

  let preserve_identity =
    let test_name = "pure id <*> x = x"
    and test_arbitrary = applicative_of x
    and test x =
      let open Applicative in
      let left = pure (fun i -> i) <*> x
      and right = x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let homomorphism =
    let test_name = "pure f <*> pure x = pure (f x)"
    and test_arbitrary = pair x (fun1 x_input f)
    and test (x, f') =
      let open Applicative in
      let f = Fn.apply f' in
      let left = pure f <*> pure x
      and right = pure (f x) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let interchange =
    let test_name = "f <*> pure x = pure ((|>) x) <*> f"
    and test_arbitrary = pair x (applicative_of (fun1 x_input u))
    and test (x, u') =
      let open Applicative in
      let u = Fn.apply <$> u' in
      let left = u <*> pure x
      and right = pure (( |> ) x) <*> u in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let composition =
    let test_name = "pure ( % ) <*> u <*> v <*> w = u <*> (v <*> w)"
    and test_arbitrary =
      triple
        (applicative_of (fun1 v_input x))
        (applicative_of (fun1 w_input v))
        (applicative_of w)
    and test (u', v', w) =
      let open Applicative in
      let open Preface_core.Fun in
      let u = Fn.apply <$> u' in
      let v = Fn.apply <$> v' in
      let left = pure ( % ) <*> u <*> v <*> w
      and right = u <*> (v <*> w) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let additional_laws_1 =
    let test_name = "u *> v = (id <$ u) <*> v"
    and test_arbitrary = applicative_of x
    and test v =
      let open Applicative in
      let left = pure () *> v
      and right = (fun x -> x) <$ pure () <*> v in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let additional_laws_2 =
    let test_name = "u <* v = lift2 const u v"
    and test_arbitrary = applicative_of x
    and test v =
      let open Applicative in
      let open Preface_core.Fun in
      let left = v <* pure ()
      and right = lift2 constant v (pure ()) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let apply =
    let test_name = "apply f a = map (fun (f, a) -> f a) (product f a)"
    and test_arbitrary = pair (applicative_of x) (fun1 x_input f)
    and test (x, f') =
      let open Applicative in
      let f = pure (Fn.apply f') in
      let left = apply f x
      and right = map (fun (f, a) -> f a) (product f x) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let map =
    let test_name = "map f a = apply (pure f) a"
    and test_arbitrary = pair (applicative_of x) (fun1 x_input f)
    and test (x, f') =
      let open Applicative in
      let f = Fn.apply f' in
      let left = map f x
      and right = apply (pure f) x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let product =
    let test_name = "product a b = apply (apply (pure (fun a  -> (a, b)))) b"
    and test_arbitrary = pair (applicative_of x) (applicative_of f)
    and test (a, b) =
      let open Applicative in
      let left = product a b
      and right = apply (apply (pure (fun a b -> (a, b))) a) b in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let lift =
    let test_name = "lift = map"
    and test_arbitrary = pair (applicative_of x) (fun1 x_input f)
    and test (x, f') =
      let open Applicative in
      let f = Fn.apply f' in
      let left = map f x
      and right = lift f x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let lift2 =
    let test_name = "lift2 f a = apply @@ apply (pure f) a"
    and test_arbitrary =
      triple (applicative_of x) (applicative_of f) (fun2 x_input f_input w)
    and test (a, b, f') =
      let open Applicative in
      let f = Fn.apply f' in
      let left = (apply @@ apply (pure f) a) b
      and right = lift2 f a b in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let lift3 =
    let test_name = "lift3 f a = apply @@ apply (apply (pure f) a) b"
    and test_arbitrary =
      quad (applicative_of x) (applicative_of f) (applicative_of u)
        (fun3 x_input f_input u_input u)
    and test (a, b, c, f') =
      let open Applicative in
      let f = Fn.apply f' in
      let left = (apply @@ apply (apply (pure f) a) b) c
      and right = lift3 f a b c in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let replace =
    let test_name = "replace = map % const"
    and test_arbitrary = pair (applicative_of x) f
    and test (x, value) =
      let open Applicative in
      let open Preface_core.Fun in
      let left = replace value x
      and right = (map % const) value x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let infix_map =
    let test_name = "<$> = map"
    and test_arbitrary = pair (fun1 x_input f) (applicative_of x)
    and test (f', x) =
      let open Applicative in
      let f = Fn.apply f' in
      let left = f <$> x
      and right = map f x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let infix_replace =
    let test_name = "%> = replace"
    and test_arbitrary = pair (applicative_of x) f
    and test (x, value) =
      let open Applicative in
      let open Preface_core.Fun in
      let left = value <$ x
      and right = (map % const) value x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let infix_rreplace =
    let test_name = "<% = flip replace"
    and test_arbitrary = pair (applicative_of x) f
    and test (x, value) =
      let open Applicative in
      let open Preface_core.Fun in
      let left = x $> value
      and right = (map % const) value x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let infix_apply =
    let test_name = "<*> = apply"
    and test_arbitrary = pair (applicative_of x) (fun1 x_input f)
    and test (x, f') =
      let open Applicative in
      let f = pure (Fn.apply f') in
      let left = apply f x
      and right = f <*> x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let infix_rapply =
    let test_name = "<**> = flip apply"
    and test_arbitrary = pair (applicative_of x) (fun1 x_input f)
    and test (x, f') =
      let open Applicative in
      let f = pure (Fn.apply f') in
      let left = apply f x
      and right = x <**> f in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let syntax_map =
    let test_name = "(let+ x = a in f x) = (f <$> a)"
    and test_arbitrary = pair (applicative_of x) (fun1 x_input f)
    and test (x, f') =
      let open Applicative in
      let f = Fn.apply f' in
      let left = map f x
      and right =
        let+ a = x in
        f a
      in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let syntax_product =
    let test_name = "(let+ x = a and+ y = b in (a, b)) = (product a b)"
    and test_arbitrary = pair (applicative_of x) (applicative_of f)
    and test (a, b) =
      let open Applicative in
      let left = product a b
      and right =
        let+ x = a
        and+ y = b in
        (x, y)
      in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let cases =
    ( Req.suite_name ^ " applicative"
    , List.map QCheck_alcotest.to_alcotest
        [
          preserve_identity
        ; homomorphism
        ; interchange
        ; composition
        ; additional_laws_1
        ; additional_laws_2
        ; apply
        ; map
        ; product
        ; lift
        ; lift2
        ; lift3
        ; replace
        ; infix_map
        ; infix_replace
        ; infix_rreplace
        ; infix_apply
        ; infix_rapply
        ; syntax_map
        ; syntax_product
        ] )
  ;;
end

module Make
    (Applicative : APPLICATIVE)
    (Req : Qcheck_helpers.REQ with type 'a t = 'a Applicative.t) =
  Make_with_post_hook (Applicative) (Req)
    (struct
      type 'a t = 'a Applicative.t

      let apply x = Obj.magic x
    end)
