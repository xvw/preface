module Category = Preface_laws.Category.Laws (Preface_stdlib.Fun.Category)
module Arrow = Preface_laws.Arrow.Laws (Preface_stdlib.Fun.Arrow)
module Choice = Preface_laws.Arrow_choice.Laws (Preface_stdlib.Fun.Arrow_choice)
module Either = Preface_stdlib.Either

let tuple_eq f g (a, b) (a', b') = f a a' && g b b'

let cat_right_identity (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Category.right_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value))
;;

let cat_left_identity (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Category.left_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value))
;;

let cat_associativity (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    quad
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.A.arbitrary)
      (fun1 P.D.observable P.C.arbitrary)
      P.D.arbitrary
  in
  let (name, test) = Category.associativity in
  Test.make ~name ~count arbitrary (fun (f', g', h', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let h = Fn.apply h' in
      let (left, right) = test f g h in
      P.B.equal (left value) (right value))
;;

let arrow_right_identity (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Arrow.right_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value))
;;

let arrow_left_identity (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Arrow.left_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value))
;;

let arrow_associativity (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    quad
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.A.arbitrary)
      (fun1 P.D.observable P.C.arbitrary)
      P.D.arbitrary
  in
  let (name, test) = Arrow.associativity in
  Test.make ~name ~count arbitrary (fun (f', g', h', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let h = Fn.apply h' in
      let (left, right) = test f g h in
      P.B.equal (left value) (right value))
;;

let arrow_law1 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = P.A.arbitrary in
  let (name, test) = Arrow.law1 in
  Test.make ~name ~count arbitrary (fun value ->
      let (left, right) = test () in
      P.A.equal (left value) (right value))
;;

let arrow_law2 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    triple
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.B.observable P.C.arbitrary)
      P.A.arbitrary
  in
  let (name, test) = Arrow.law2 in
  Test.make ~name ~count arbitrary (fun (f', g', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let (left, right) = test f g in
      P.C.equal (left value) (right value))
;;

let arrow_law3 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair (fun1 P.A.observable P.B.arbitrary) (pair P.A.arbitrary P.C.arbitrary)
  in
  let (name, test) = Arrow.law3 in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      tuple_eq P.B.equal P.C.equal (left value) (right value))
;;

let arrow_law4 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    triple
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.B.observable P.C.arbitrary)
      (pair P.A.arbitrary P.D.arbitrary)
  in
  let (name, test) = Arrow.law4 in
  Test.make ~name ~count arbitrary (fun (f', g', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let (left, right) = test f g in
      tuple_eq P.C.equal P.D.equal (left value) (right value))
;;

let arrow_law5 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    triple
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.D.arbitrary)
      (pair P.A.arbitrary P.C.arbitrary)
  in
  let (name, test) = Arrow.law5 in
  Test.make ~name ~count arbitrary (fun (f', g', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let (left, right) = test f g in
      tuple_eq P.B.equal P.D.equal (left value) (right value))
;;

let arrow_law6 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair (fun1 P.A.observable P.B.arbitrary) (pair P.A.arbitrary P.C.arbitrary)
  in
  let (name, test) = Arrow.law6 in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value))
;;

let arrow_law7 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair
      (fun1 P.A.observable P.B.arbitrary)
      (pair (pair P.A.arbitrary P.C.arbitrary) P.D.arbitrary)
  in
  let (name, test) = Arrow.law7 in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Preface_stdlib.Fun.Arrow.arrow (Fn.apply f') in
      let (left, right) = test f in
      tuple_eq P.B.equal
        (tuple_eq P.C.equal P.D.equal)
        (left value) (right value))
;;

let arrow_choice_right_identity (module P : Preface_qcheck.Sample.PACKAGE) count
    =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Choice.right_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value))
;;

let arrow_choice_left_identity (module P : Preface_qcheck.Sample.PACKAGE) count
    =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Choice.left_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value))
;;

let arrow_choice_associativity (module P : Preface_qcheck.Sample.PACKAGE) count
    =
  let open QCheck in
  let arbitrary =
    quad
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.A.arbitrary)
      (fun1 P.D.observable P.C.arbitrary)
      P.D.arbitrary
  in
  let (name, test) = Choice.associativity in
  Test.make ~name ~count arbitrary (fun (f', g', h', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let h = Fn.apply h' in
      let (left, right) = test f g h in
      P.B.equal (left value) (right value))
;;

let arrow_choice_law1 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = P.A.arbitrary in
  let (name, test) = Choice.law1 in
  Test.make ~name ~count arbitrary (fun value ->
      let (left, right) = test () in
      P.A.equal (left value) (right value))
;;

let arrow_choice_law2 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    triple
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.B.observable P.C.arbitrary)
      P.A.arbitrary
  in
  let (name, test) = Choice.law2 in
  Test.make ~name ~count arbitrary (fun (f', g', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let (left, right) = test f g in
      P.C.equal (left value) (right value))
;;

let arrow_choice_law3 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair (fun1 P.A.observable P.B.arbitrary) (pair P.A.arbitrary P.C.arbitrary)
  in
  let (name, test) = Choice.law3 in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      tuple_eq P.B.equal P.C.equal (left value) (right value))
;;

let arrow_choice_law4 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    triple
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.B.observable P.C.arbitrary)
      (pair P.A.arbitrary P.D.arbitrary)
  in
  let (name, test) = Choice.law4 in
  Test.make ~name ~count arbitrary (fun (f', g', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let (left, right) = test f g in
      tuple_eq P.C.equal P.D.equal (left value) (right value))
;;

let arrow_choice_law5 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    triple
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.D.arbitrary)
      (pair P.A.arbitrary P.C.arbitrary)
  in
  let (name, test) = Choice.law5 in
  Test.make ~name ~count arbitrary (fun (f', g', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let (left, right) = test f g in
      tuple_eq P.B.equal P.D.equal (left value) (right value))
;;

let arrow_choice_law6 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair (fun1 P.A.observable P.B.arbitrary) (pair P.A.arbitrary P.C.arbitrary)
  in
  let (name, test) = Choice.law6 in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value))
;;

let arrow_choice_law7 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair
      (fun1 P.A.observable P.B.arbitrary)
      (pair (pair P.A.arbitrary P.C.arbitrary) P.D.arbitrary)
  in
  let (name, test) = Choice.law7 in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Preface_stdlib.Fun.Arrow_choice.arrow (Fn.apply f') in
      let (left, right) = test f in
      tuple_eq P.B.equal
        (tuple_eq P.C.equal P.D.equal)
        (left value) (right value))
;;

let either l r = Preface_qcheck.Arbitrary.either l r

let arrow_choice_law8 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair
      (fun1 P.A.observable P.B.arbitrary)
      (either P.A.arbitrary P.C.arbitrary)
  in
  let (name, test) = Choice.law8 in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      Either.equal P.B.equal P.C.equal (left value) (right value))
;;

let arrow_choice_law9 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    triple
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.B.observable P.C.arbitrary)
      (either P.A.arbitrary P.D.arbitrary)
  in
  let (name, test) = Choice.law9 in
  Test.make ~name ~count arbitrary (fun (f', g', value) ->
      let f = Preface_stdlib.Fun.Arrow_choice.arrow (Fn.apply f') in
      let g = Preface_stdlib.Fun.Arrow_choice.arrow (Fn.apply g') in
      let (left, right) = test f g in
      Either.equal P.C.equal P.D.equal (left value) (right value))
;;

let arrow_choice_law10 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Choice.law10 in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Preface_stdlib.Fun.Arrow_choice.arrow (Fn.apply f') in
      let (left, right) = test f in
      Either.equal P.B.equal P.C.equal (left value) (right value))
;;

let arrow_choice_law11 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    triple
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.D.arbitrary)
      (either P.A.arbitrary P.C.arbitrary)
  in
  let (name, test) = Choice.law11 in
  Test.make ~name ~count arbitrary (fun (f', g', value) ->
      let f = Preface_stdlib.Fun.Arrow_choice.arrow (Fn.apply f') in
      let g = Fn.apply g' in
      let (left, right) = test f g in
      Either.equal P.B.equal P.D.equal (left value) (right value))
;;

let arrow_choice_law12 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair
      (fun1 P.A.observable P.B.arbitrary)
      (either (either P.A.arbitrary P.C.arbitrary) P.D.arbitrary)
  in
  let (name, test) = Choice.law12 in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Preface_stdlib.Fun.Arrow_choice.arrow (Fn.apply f') in
      let (left, right) = test f in
      Either.equal P.B.equal
        (Either.equal P.C.equal P.D.equal)
        (left value) (right value))
;;

let cases n =
  [
    ( "Fun Category Laws"
    , [
        cat_right_identity (module Preface_qcheck.Sample.Pack1) n
      ; cat_left_identity (module Preface_qcheck.Sample.Pack1) n
      ; cat_associativity (module Preface_qcheck.Sample.Pack1) n
      ]
      |> Stdlib.List.map QCheck_alcotest.to_alcotest )
  ; ( "Fun Arrow Laws"
    , [
        arrow_right_identity (module Preface_qcheck.Sample.Pack1) n
      ; arrow_left_identity (module Preface_qcheck.Sample.Pack1) n
      ; arrow_associativity (module Preface_qcheck.Sample.Pack1) n
      ; arrow_law1 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_law2 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_law3 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_law4 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_law5 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_law6 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_law7 (module Preface_qcheck.Sample.Pack1) n
      ]
      |> Stdlib.List.map QCheck_alcotest.to_alcotest )
  ; ( "Fun Arrow Choice Laws"
    , [
        arrow_choice_right_identity (module Preface_qcheck.Sample.Pack1) n
      ; arrow_choice_left_identity (module Preface_qcheck.Sample.Pack1) n
      ; arrow_choice_associativity (module Preface_qcheck.Sample.Pack1) n
      ; arrow_choice_law1 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_choice_law2 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_choice_law3 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_choice_law4 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_choice_law5 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_choice_law6 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_choice_law7 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_choice_law8 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_choice_law9 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_choice_law10 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_choice_law11 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_choice_law12 (module Preface_qcheck.Sample.Pack1) n
      ]
      |> Stdlib.List.map QCheck_alcotest.to_alcotest )
  ]
;;
