let preserve_identity (module P : Preface_qcheck.Model.T0) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.observable bool) P.arbitrary in
  Test.make ~name:"map id = id" ~count arbitrary (fun (f', value) ->
      let open Preface_stdlib.Predicate in
      let f = Fn.apply f' in
      let l = (Contravariant.contramap (fun x -> x) f) value
      and r = f value in
      Bool.equal l r)
;;

let preserve_morphism (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    quad (fun1 P.A.observable bool)
      (fun1 P.C.observable P.D.arbitrary)
      (fun1 P.D.observable P.A.arbitrary)
      P.C.arbitrary
  in
  Test.make ~name:"contramap (f % g) = contramap g % contramap f" ~count
    arbitrary (fun (x', f', g', value) ->
      let open Preface_stdlib.Predicate in
      let open Preface_core.Fun.Infix in
      let x = Fn.apply x' in
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let l = (Contravariant.contramap (g % f) x) value
      and r =
        ((Contravariant.contramap f % Contravariant.contramap g) x) value
      in
      Bool.equal l r)
;;

let cases n =
  [
    ( "Predicate Contravariant Functor Laws"
    , [
        preserve_identity (module Preface_qcheck.Sample.Pack1.A) n
      ; preserve_morphism (module Preface_qcheck.Sample.Pack1) n
      ]
      |> Stdlib.List.map QCheck_alcotest.to_alcotest )
  ]
;;
