module Make
    (S : Preface_specs.SELECTIVE)
    (R : Requirement.INPUT_T1 with type 'a t = 'a S.t)
    (P : Sample.PACK) : Requirement.OUTPUT = struct
  open QCheck

  open Helper.Make_for_t1 (R) (P)

  module Underlying_functor = Preface_make.Functor.Via_map (S)

  module Select :
    Preface_specs.Selective.CORE_WITH_SELECT
      with type 'a t = 'a Underlying_functor.t = struct
    type 'a t = 'a Underlying_functor.t

    let pure = S.pure

    let select = S.select
  end

  module Underlying =
    Preface_make.Selective.Over_functor (Underlying_functor) (Select)

  let either l r = Arbitrary.either l r

  let identity =
    let test_name = "x <*? pure id = Either.case id id <$> x"
    and test_arbitrary = over (either t1 t1)
    and test x =
      let open Preface_core.Fun in
      let open S in
      let left = x <*? pure id in
      let right = Preface_core.Shims.Either.case id id <$> x in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let distributive =
    let test_name = "pure x <*? (y *> z) = (pure x <*? y) *> (pure x <*? z)"
    and test_arbitrary =
      triple (either t1 t2) (over (fun1 t1' t2)) (over (fun1 t1' t2))
    and test (x, y', z') =
      let open S in
      let y = Fn.apply <$> y' in
      let z = Fn.apply <$> z' in
      let left = pure x <*? replace () y *> z
      and right = replace () (pure x <*? y) *> (pure x <*? z) in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let associativity =
    let test_name = "x <*? (y <*? z) = (f <$> x) <*? (g <$> y) <*? (h <$> z)"
    and test_arbitrary =
      triple
        (over (either t2 t3))
        (over (either t1 (fun1 t2' t3)))
        (over (fun2 t1' t2' t3))
    and test (x, y', z') =
      let open S in
      let f a = Preface_stdlib.Either.map_right (fun x -> Either.Right x) a
      and g x a =
        Preface_stdlib.Either.Bifunctor.bimap (fun x -> (x, a)) (fun f -> f a) x
      and h x (a, b) = x a b in
      let y = Either.map_right Fn.apply <$> y' in
      let z = Fn.apply <$> z' in
      let left = x <*? (y <*? z)
      and right = f <$> x <*? (g <$> y) <*? (h <$> z) in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let theorem1 =
    let test_name =
      "(f <$> select x y) = (select (Bifunctor.snd f <$> x) (((%) f) <$> y))"
    and test_arbitrary =
      triple (fun1 t1' t3) (over (either t2 t1)) (over (fun1 t2' t1))
    and test (f', x, y') =
      let open Preface_core.Fun in
      let open S in
      let f = Fn.apply f'
      and y = Fn.apply <$> y' in
      let left = f <$> select x y
      and right =
        select (Preface_stdlib.Either.Bifunctor.snd f <$> x) (( % ) f <$> y)
      in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let theorem2 =
    let test_name =
      "(select (Bifunctor.fst f <$> x) y) = (select x ((%>) f) <$> y))"
    and test_arbitrary =
      triple (fun1 t1' t2) (over (either t1 t3)) (over (fun1 t2' t3))
    and test (f', x, y') =
      let open Preface_core.Fun in
      let open S in
      let f = Fn.apply f' in
      let y = Fn.apply <$> y' in
      let left = select (Preface_stdlib.Either.Bifunctor.fst f <$> x) y
      and right = select x (( %> ) f <$> y) in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let theorem3 =
    let test_name =
      "(select x (f <$> y)) = (select (Bifunctor.fst (flip f) <$> x) ((|>) <$> \
       y))"
    and test_arbitrary =
      triple (fun2 t1' t2' t3) (over (either t2 t3)) (over t1)
    and test (f', x, y) =
      let flip f x y = f y x in
      let open S in
      let f = Fn.apply f' in
      let left = select x (f <$> y)
      and right =
        select
          (Preface_stdlib.Either.Bifunctor.fst (flip f) <$> x)
          (( |> ) <$> y)
      in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let theorem4 =
    let test_name = "(x <*? pure y) = (Either.case y id <$> x)"
    and test_arbitrary = pair (over (either t1 t2)) (fun1 t1' t2)
    and test (x, y') =
      let open S in
      let y = Fn.apply y' in
      let left = x <*? pure y
      and right = Preface_core.Shims.Either.case y Preface_core.Fun.id <$> x in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let property_pure_right =
    let test_name = "(pure (Right x) <*? y) = pure x"
    and test_arbitrary = pair t1 (over (fun1 t2' t1))
    and test (x, y') =
      let open S in
      let y = Fn.apply <$> y' in
      let left = pure (Either.Right x) <*? y
      and right = pure x in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let property_pure_left =
    let test_name = "(pure (Left x) <*? y) = ((|>) x) <$> y"
    and test_arbitrary = pair t1 (over (fun1 t1' t2))
    and test (x, y') =
      let open S in
      let y = Fn.apply <$> y' in
      let left = pure (Either.Left x) <*? y
      and right = ( |> ) x <$> y in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let branch =
    let test_name = "branch"
    and test_arbitrary =
      triple (over (either t1 t2)) (over (fun1 t1' t3)) (over (fun1 t2' t3))
    and test (x, f', g') =
      let open S in
      let f = Fn.apply <$> f'
      and g = Fn.apply <$> g' in
      let left = branch x f g
      and right = Underlying.branch x f g in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let if_ =
    let test_name = "if_"
    and test_arbitrary = triple (over bool) (over t1) (over t1)
    and test (x, a, b) =
      let left = S.if_ x a b
      and right = Underlying.if_ x a b in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let bind_bool =
    let test_name = "bind_bool"
    and test_arbitrary = pair (over bool) (fun1 Observable.bool (over t1))
    and test (x, f') =
      let f = Fn.apply f' in
      let left = S.bind_bool x f
      and right = Underlying.bind_bool x f in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let extra_small_list = list_of_size (Gen.int_range 0 4)

  let exists =
    let test_name = "exists"
    and test_arbitrary = pair (fun1 t1' (over bool)) (extra_small_list t1)
    and test (f', xs) =
      let f = Fn.apply f' in
      let left = S.exists f xs
      and right = Underlying.exists f xs in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let for_all =
    let test_name = "for_all"
    and test_arbitrary = pair (fun1 t1' (over bool)) (extra_small_list t1)
    and test (f', xs) =
      let f = Fn.apply f' in
      let left = S.for_all f xs
      and right = Underlying.for_all f xs in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let or_ =
    let test_name = "or_"
    and test_arbitrary = pair (over bool) (over bool)
    and test (l, r) =
      let left = S.or_ l r
      and right = Underlying.or_ l r in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let and_ =
    let test_name = "and_"
    and test_arbitrary = pair (over bool) (over bool)
    and test (l, r) =
      let left = S.and_ l r
      and right = Underlying.and_ l r in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let infix_or_ =
    let test_name = "<||>"
    and test_arbitrary = pair (over bool) (over bool)
    and test (l, r) =
      let left = S.(l <||> r)
      and right = Underlying.(l <||> r) in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let infix_and_ =
    let test_name = "<&&>"
    and test_arbitrary = pair (over bool) (over bool)
    and test (l, r) =
      let left = S.(l <&&> r)
      and right = Underlying.(l <&&> r) in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let infix_select =
    let test_name = "<*?"
    and test_arbitrary = pair (over (either t1 t2)) (over (fun1 t1' t2))
    and test (x, f') =
      let open S in
      let f = Fn.apply <$> f' in
      let left = x <*? f
      and right = Underlying.(x <*? f) in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  module Applicative_test =
    Applicative.Make
      (S)
      (struct
        include R

        let name = "Selective of " ^ R.name
      end)
      (P)

  let cases =
    [
      ( "Selective " ^ R.name ^ " laws"
      , [
          identity
        ; distributive
        ; associativity
        ; theorem1
        ; theorem2
        ; theorem3
        ; theorem4
        ; property_pure_left
        ; property_pure_right
        ]
        |> List.map QCheck_alcotest.to_alcotest )
    ; ( "Selective " ^ R.name ^ " has expected behaviour"
      , [
          branch
        ; if_
        ; bind_bool
        ; exists
        ; for_all
        ; or_
        ; and_
        ; infix_and_
        ; infix_or_
        ; infix_select
        ]
        |> List.map QCheck_alcotest.to_alcotest )
    ]
    @ Applicative_test.cases
  ;;
end

module Make_rigid
    (S : Preface_specs.SELECTIVE)
    (R : Requirement.INPUT_T1 with type 'a t = 'a S.t)
    (P : Sample.PACK) : Requirement.OUTPUT = struct
  open QCheck
  module Base = Make (S) (R) (P)

  open Helper.Make_for_t1 (R) (P)

  let either l r = Arbitrary.either l r

  let theorem5 =
    let test_name = "f <*> g = apply f g"
    and test_arbitrary = pair (over (fun1 t1' t2)) (over t1)
    and test (f', g) =
      let open S in
      let f = Fn.apply <$> f' in
      let left = f <*> g
      and right = apply f g in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let theorem6 =
    let test_name = "(x *> (y <*? z)) = ((x *> y) <*? z)"
    and test_arbitrary =
      triple (over t1) (over (either t2 t3)) (over (fun1 t2' t3))
    and test (x, y, z') =
      let open S in
      let z = Fn.apply <$> z' in
      let left =
        let b = y <*? z in
        let a = ignore <$> x in
        a *> b
      and right =
        let a = ignore <$> x in
        let b = a *> y in
        b <*? z
      in
      left = right
    in
    Test.make ~count:R.size ~name:test_name test_arbitrary test
  ;;

  let cases =
    Base.cases
    @ [
        ( "Selective Rigid " ^ R.name ^ " Laws"
        , [ theorem5; theorem6 ] |> List.map QCheck_alcotest.to_alcotest )
      ]
  ;;
end
