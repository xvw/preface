module Make_hooked
    (A : Preface_specs.APPLICATIVE)
    (R : Requirement.INPUT_T1 with type 'a t = 'a A.t)
    (Hook : Requirement.HOOK with type 'a t = 'a A.t)
    (P : Sample.PACK) : Requirement.OUTPUT = struct
  open QCheck

  open Helper.Make_for_t1 (R) (P)

  module Underlying = Preface_make.Applicative.Via_apply (A)

  (* Test Applicative laws *)

  let preserve_identity =
    let test_name = "pure id <*> x = x"
    and test_arbitrary = over t1
    and test x =
      let open A in
      let left = pure (fun i -> i) <*> x
      and right = x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let homomorphism =
    let test_name = "pure f <*> pure x = pure (f x)"
    and test_arbitrary = pair t1 (fun1 t1' t2)
    and test (x, f') =
      let open A in
      let f = Fn.apply f' in
      let left = pure f <*> pure x
      and right = pure (f x) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let interchange =
    let test_name = "f <*> pure x = pure ((|>) x) <*> f"
    and test_arbitrary = pair t1 (over (fun1 t1' t2))
    and test (x, u') =
      let open A in
      let u = Fn.apply <$> u' in
      let left = u <*> pure x
      and right = pure (( |> ) x) <*> u in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let composition =
    let test_name = "pure ( % ) <*> u <*> v <*> w = u <*> (v <*> w)"
    and test_arbitrary =
      triple (over (fun1 t1' t3)) (over (fun1 t2' t1)) (over t2)
    and test (u', v', w) =
      let open A in
      let open Preface_core.Fun in
      let u = Fn.apply <$> u' in
      let v = Fn.apply <$> v' in
      let left = pure ( % ) <*> u <*> v <*> w
      and right = u <*> (v <*> w) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let additional_laws_1 =
    let test_name = "u *> v = (id <$ u) <*> v"
    and test_arbitrary = over t1
    and test v =
      let open A in
      let left = pure () *> v
      and right = (fun x -> x) <$ pure () <*> v in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let additional_laws_2 =
    let test_name = "u <* v = lift2 const u v"
    and test_arbitrary = over t1
    and test v =
      let open A in
      let open Preface_core.Fun in
      let left = v <* pure ()
      and right = lift2 constant v (pure ()) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  (* Test underlying Applicative *)

  let apply =
    let test_name = "apply"
    and test_arbitrary = pair (over t1) (fun1 t1' t2)
    and test (x, f') =
      let f = A.pure (Fn.apply f') in
      let left = A.(apply f x)
      and right = Underlying.(apply f x) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let map =
    let test_name = "map"
    and test_arbitrary = pair (over t1) (fun1 t1' t2)
    and test (x, f') =
      let f = Fn.apply f' in
      let left = A.(map f x)
      and right = Underlying.(map f x) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let product =
    let test_name = "product"
    and test_arbitrary = pair (over t1) (over t2)
    and test (a, b) =
      let left = A.product a b
      and right = Underlying.product a b in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let lift =
    let test_name = "lift"
    and test_arbitrary = pair (over t1) (fun1 t1' t2)
    and test (x, f') =
      let f = Fn.apply f' in
      let left = A.lift f x
      and right = Underlying.lift f x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let lift2 =
    let test_name = "lift2"
    and test_arbitrary = triple (over t1) (over t2) (fun2 t1' t2' t3)
    and test (a, b, f') =
      let f = Fn.apply f' in
      let left = A.lift2 f a b
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
      let left = A.lift3 f a b c
      and right = Underlying.lift3 f a b c in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let replace =
    let test_name = "replace"
    and test_arbitrary = pair (over t1) t2
    and test (x, value) =
      let left = A.replace value x
      and right = Underlying.replace value x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let infix_map =
    let test_name = "<$>"
    and test_arbitrary = pair (fun1 t1' t2) (over t1)
    and test (f', x) =
      let f = Fn.apply f' in
      let left = A.(f <$> x)
      and right = Underlying.(f <$> x) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let infix_replace =
    let test_name = "<$"
    and test_arbitrary = pair (over t1) t2
    and test (x, value) =
      let left = A.(value <$ x)
      and right = Underlying.(value <$ x) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let infix_rreplace =
    let test_name = "$>"
    and test_arbitrary = pair (over t1) t2
    and test (x, value) =
      let left = A.(x $> value)
      and right = Underlying.(x $> value) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let infix_apply =
    let test_name = "<*>"
    and test_arbitrary = pair (over t1) (fun1 t1' t2)
    and test (x, f') =
      let f = A.pure (Fn.apply f') in
      let left = A.(f <*> x)
      and right = Underlying.(f <*> x) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let infix_rapply =
    let test_name = "<**> = flip apply"
    and test_arbitrary = pair (over t1) (fun1 t1' t2)
    and test (x, f') =
      let f = A.pure (Fn.apply f') in
      let left = A.(x <**> f)
      and right = Underlying.(x <**> f) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let syntax_map =
    let test_name = "let+"
    and test_arbitrary = pair (over t1) (fun1 t1' t2)
    and test (x, f') =
      let f = Fn.apply f' in
      let left =
        let open A in
        let+ a = x in
        f a
      and right =
        let open Underlying in
        let+ a = x in
        f a
      in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let syntax_product =
    let test_name = "let+, and+"
    and test_arbitrary = pair (over t1) (over t2)
    and test (a, b) =
      let left =
        let open A in
        let+ x = a
        and+ y = b in
        (x, y)
      and right =
        let open Underlying in
        let+ x = a
        and+ y = b in
        (x, y)
      in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let cases =
    [
      ( "Applicative " ^ R.name ^ " laws"
      , [
          preserve_identity
        ; homomorphism
        ; interchange
        ; composition
        ; additional_laws_1
        ; additional_laws_2
        ]
        |> List.map QCheck_alcotest.to_alcotest )
    ; ( "Applicative " ^ R.name ^ " has expected behaviour"
      , [
          apply
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
        ]
        |> List.map QCheck_alcotest.to_alcotest )
    ]
  ;;
end

module Make
    (A : Preface_specs.APPLICATIVE)
    (R : Requirement.INPUT_T1 with type 'a t = 'a A.t) =
  Make_hooked (A) (R)
    (struct
      type 'a t = 'a A.t

      let apply x = Obj.magic x
    end)
