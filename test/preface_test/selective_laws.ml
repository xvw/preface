(* Test widely adapted from: 
   https://github.com/snowleopard/selective/blob/master/test/Laws.hs
*)

module type SELECTIVE = Preface_specs.SELECTIVE

module Either = Preface_stdlib.Either

module Make
    (Selective : SELECTIVE with type ('a, 'b) either = ('a, 'b) Either.t)
    (Rigid : sig
      val is : bool
    end)
    (Req : Qcheck_helpers.REQ with type 'a t = 'a Selective.t)
    (X : Qcheck_helpers.GENERATOR)
    (Y : Qcheck_helpers.GENERATOR)
    (Z : Qcheck_helpers.GENERATOR) : Qcheck_helpers.ALCOTEST_SUITE = struct
  open QCheck

  let selective_of = Req.arbitrary

  let x = X.arbitrary

  let x_input = X.observable

  let y = Y.arbitrary

  let z = Z.arbitrary

  let y_input = Y.observable

  let either l r = Qcheck_helpers.Arbitrary.either l r

  let identity =
    let test_name = "x <*? pure id = Either.case id id <$> x"
    and test_arbitrary = selective_of (either x x)
    and test x =
      let open Preface_core.Fun in
      let open Selective in
      let left = x <*? pure id in
      let right = Either.case id id <$> x in
      left = right
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let distributive =
    let test_name = "pure x <*? (y *> z) = (pure x <*? y) *> (pure x <*? z)"
    and test_arbitrary =
      triple (either x y)
        (selective_of (fun1 x_input y))
        (selective_of (fun1 x_input y))
    and test (x, y', z') =
      let open Selective in
      let y = Fn.apply <$> y' in
      let z = Fn.apply <$> z' in
      let left = pure x <*? replace () y *> z
      and right = replace () (pure x <*? y) *> (pure x <*? z) in
      left = right
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let associativity =
    let test_name = "x <*? (y <*? z) = (f <$> x) <*? (g <$> y) <*? (h <$> z)"
    and test_arbitrary =
      triple
        (selective_of (either y z))
        (selective_of (either x (fun1 y_input z)))
        (selective_of (fun2 x_input y_input z))
    and test (x, y', z') =
      let open Selective in
      let f a = Either.map_right (fun x -> Either.Right x) a
      and g x a = Either.Bifunctor.bimap (fun x -> (x, a)) (fun f -> f a) x
      and h x (a, b) = x a b in
      let y = Either.map_right Fn.apply <$> y' in
      let z = Fn.apply <$> z' in
      let left = x <*? (y <*? z)
      and right = f <$> x <*? (g <$> y) <*? (h <$> z) in
      left = right
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let theorem1 =
    let test_name =
      "(f <$> select x y) = (select (Bifunctor.snd f <$> x) (((%) f) <$> y))"
    and test_arbitrary =
      triple (fun1 x_input z)
        (selective_of (either y x))
        (selective_of (fun1 y_input x))
    and test (f', x, y') =
      let open Preface_core.Fun in
      let open Selective in
      let f = Fn.apply f'
      and y = Fn.apply <$> y' in
      let left = f <$> select x y
      and right = select (Either.Bifunctor.snd f <$> x) (( % ) f <$> y) in
      left = right
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let theorem2 =
    let test_name =
      "(select (Bifunctor.fst f <$> x) y) = (select x ((%>) f) <$> y))"
    and test_arbitrary =
      triple (fun1 x_input y)
        (selective_of (either x z))
        (selective_of (fun1 y_input z))
    and test (f', x, y') =
      let open Preface_core.Fun in
      let open Selective in
      let f = Fn.apply f' in
      let y = Fn.apply <$> y' in
      let left = select (Either.Bifunctor.fst f <$> x) y
      and right = select x (( %> ) f <$> y) in
      left = right
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let theorem3 =
    let test_name =
      "(select x (f <$> y)) = (select (Bifunctor.fst (flip f) <$> x) ((|>) <$> \
       y))"
    and test_arbitrary =
      triple (fun2 x_input y_input z)
        (selective_of (either y z))
        (selective_of x)
    and test (f', x, y) =
      let flip f x y = f y x in
      let open Selective in
      let f = Fn.apply f' in
      let left = select x (f <$> y)
      and right = select (Either.Bifunctor.fst (flip f) <$> x) (( |> ) <$> y) in
      left = right
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let theorem4 =
    let test_name = "(x <*? pure y) = (Either.case y id <$> x)"
    and test_arbitrary = pair (selective_of (either x y)) (fun1 x_input y)
    and test (x, y') =
      let open Selective in
      let y = Fn.apply y' in
      let left = x <*? pure y
      and right = Either.case y Preface_core.Fun.id <$> x in
      left = right
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let theorem5 =
    let test_name = "f <*> g = apply f g"
    and test_arbitrary = pair (selective_of (fun1 x_input y)) (selective_of x)
    and test (f', g) =
      if Rigid.is
      then
        let open Selective in
        let f = Fn.apply <$> f' in
        let left = f <*> g
        and right = apply f g in
        left = right
      else true
    in
    Test.make
      ~count:(if Rigid.is then 100 else 1)
      ~name:test_name test_arbitrary test
  ;;

  let theorem6 =
    let test_name = "(x *> (y <*? z)) = ((x *> y) <*? z)"
    and test_arbitrary =
      triple (selective_of x)
        (selective_of (either y z))
        (selective_of (fun1 y_input z))
    and test (x, y, z') =
      if Rigid.is
      then
        let open Selective in
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
      else true
    in
    Test.make
      ~count:(if Rigid.is then 100 else 1)
      ~name:test_name test_arbitrary test
  ;;

  let property_pure_right =
    let test_name = "(pure (Right x) <*? y) = pure x"
    and test_arbitrary = pair x (selective_of (fun1 y_input x))
    and test (x, y') =
      let open Selective in
      let y = Fn.apply <$> y' in
      let left = pure (Either.Right x) <*? y
      and right = pure x in
      left = right
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  let property_pure_left =
    let test_name = "(pure (Left x) <*? y) = ((|>) x) <$> y"
    and test_arbitrary = pair x (selective_of (fun1 x_input y))
    and test (x, y') =
      let open Selective in
      let y = Fn.apply <$> y' in
      let left = pure (Either.Left x) <*? y
      and right = ( |> ) x <$> y in
      left = right
    in
    Test.make ~count:100 ~name:test_name test_arbitrary test
  ;;

  module Applicative =
    Applicative_laws.Make (Selective) (Req) (X) (Y) (Z) (X) (Y)

  let app_cases = snd Applicative.cases

  let cases =
    ( Req.suite_name ^ " selective"
    , List.map
        (QCheck_alcotest.to_alcotest ~verbose:true ~long:true)
        [
          identity
        ; distributive
        ; associativity
        ; theorem1
        ; theorem2
        ; theorem3
        ; theorem4
        ; theorem5
        ; theorem6
        ; property_pure_right
        ; property_pure_left
        ]
      @ app_cases )
  ;;
end

module Make_for_rigid
    (Selective : SELECTIVE with type ('a, 'b) either = ('a, 'b) Either.t) =
  Make
    (Selective)
    (struct
      let is = true
    end)

module Make_for_non_rigid
    (Selective : SELECTIVE with type ('a, 'b) either = ('a, 'b) Either.t) =
  Make
    (Selective)
    (struct
      let is = false
    end)
