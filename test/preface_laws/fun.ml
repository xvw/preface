module Category = Preface_laws.Category.Laws (Preface_stdlib.Fun.Category)
module Arrow = Preface_laws.Arrow.Laws (Preface_stdlib.Fun.Arrow)
module Arrow_choice =
  Preface_laws.Arrow_choice.Laws (Preface_stdlib.Fun.Arrow_choice)
module Arrow_apply =
  Preface_laws.Arrow_apply.Laws (Preface_stdlib.Fun.Arrow_apply)
module Profunctor = Preface_laws.Profunctor.Laws (Preface_stdlib.Fun.Profunctor)
module Strong = Preface_laws.Strong.Laws (Preface_stdlib.Fun.Strong)
module Choice = Preface_laws.Choice.Laws (Preface_stdlib.Fun.Choice)
module Closed = Preface_laws.Closed.Laws (Preface_stdlib.Fun.Closed)
module Either = Preface_stdlib.Either

let tuple_eq f g (a, b) (a', b') = f a a' && g b b'

let either_eq left right = Preface_core.Shims.Either.equal ~left ~right

let either l r = Preface_qcheck.Arbitrary.either l r

let pro_dimap_id (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Profunctor.dimap_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
;;

let pro_fst_id (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Profunctor.contramap_fst_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
;;

let pro_snd_id (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Profunctor.map_snd_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
;;

let pro_dimap_eq (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    quad
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.D.arbitrary)
      (fun1 P.B.observable P.C.arbitrary)
      P.A.arbitrary
  in
  let (name, test) = Profunctor.dimap_equality in
  Test.make ~name ~count arbitrary (fun (f', g', pro', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let p = Fn.apply pro' in
      let (left, right) = test f g p in
      P.D.equal (left value) (right value) )
;;

let pro_dimap_param (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    quad
      (pair
         (fun1 P.A.observable P.B.arbitrary)
         (fun1 P.C.observable P.A.arbitrary) )
      (pair
         (fun1 P.D.observable P.E.arbitrary)
         (fun1 P.F.observable P.D.arbitrary) )
      (fun1 P.B.observable P.F.arbitrary)
      P.C.arbitrary
  in
  let (name, test) = Profunctor.dimap_parametricity in
  Test.make ~name ~count arbitrary (fun ((f', g'), (h', i'), pro', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let h = Fn.apply h' in
      let i = Fn.apply i' in
      let p = Fn.apply pro' in
      let (left, right) = test f g h i p in
      P.E.equal (left value) (right value) )
;;

let pro_fst_param (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    quad
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.A.arbitrary)
      (fun1 P.B.observable P.D.arbitrary)
      P.C.arbitrary
  in
  let (name, test) = Profunctor.contramap_fst_parametricity in
  Test.make ~name ~count arbitrary (fun (f', g', pro', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let p = Fn.apply pro' in
      let (left, right) = test f g p in
      P.D.equal (left value) (right value) )
;;

let pro_snd_param (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    quad
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.A.arbitrary)
      (fun1 P.D.observable P.C.arbitrary)
      P.D.arbitrary
  in
  let (name, test) = Profunctor.map_snd_parametricity in
  Test.make ~name ~count arbitrary (fun (f', g', pro', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let p = Fn.apply pro' in
      let (left, right) = test f g p in
      P.B.equal (left value) (right value) )
;;

let strong_dimap_id (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Strong.dimap_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
;;

let strong_fst_id (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Strong.contramap_fst_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
;;

let strong_snd_id (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Strong.map_snd_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
;;

let strong_dimap_eq (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    quad
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.D.arbitrary)
      (fun1 P.B.observable P.C.arbitrary)
      P.A.arbitrary
  in
  let (name, test) = Strong.dimap_equality in
  Test.make ~name ~count arbitrary (fun (f', g', strong', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let p = Fn.apply strong' in
      let (left, right) = test f g p in
      P.D.equal (left value) (right value) )
;;

let strong_dimap_param (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    quad
      (pair
         (fun1 P.A.observable P.B.arbitrary)
         (fun1 P.C.observable P.A.arbitrary) )
      (pair
         (fun1 P.D.observable P.E.arbitrary)
         (fun1 P.F.observable P.D.arbitrary) )
      (fun1 P.B.observable P.F.arbitrary)
      P.C.arbitrary
  in
  let (name, test) = Strong.dimap_parametricity in
  Test.make ~name ~count arbitrary (fun ((f', g'), (h', i'), strong', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let h = Fn.apply h' in
      let i = Fn.apply i' in
      let p = Fn.apply strong' in
      let (left, right) = test f g h i p in
      P.E.equal (left value) (right value) )
;;

let strong_fst_param (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    quad
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.A.arbitrary)
      (fun1 P.B.observable P.D.arbitrary)
      P.C.arbitrary
  in
  let (name, test) = Strong.contramap_fst_parametricity in
  Test.make ~name ~count arbitrary (fun (f', g', strong', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let p = Fn.apply strong' in
      let (left, right) = test f g p in
      P.D.equal (left value) (right value) )
;;

let strong_snd_param (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    quad
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.A.arbitrary)
      (fun1 P.D.observable P.C.arbitrary)
      P.D.arbitrary
  in
  let (name, test) = Strong.map_snd_parametricity in
  Test.make ~name ~count arbitrary (fun (f', g', strong', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let p = Fn.apply strong' in
      let (left, right) = test f g p in
      P.B.equal (left value) (right value) )
;;

let strong_fst_define_snd (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair (fun1 P.A.observable P.B.arbitrary) (pair P.A.arbitrary P.C.arbitrary)
  in
  let (name, test) = Strong.fst_define_snd in
  Test.make ~name ~count arbitrary (fun (strong', value) ->
      let p = Fn.apply strong' in
      let (left, right) = test p in
      tuple_eq P.B.equal P.C.equal (left value) (right value) )
;;

let strong_snd_define_fst (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair (fun1 P.A.observable P.B.arbitrary) (pair P.C.arbitrary P.A.arbitrary)
  in
  let (name, test) = Strong.snd_define_fst in
  Test.make ~name ~count arbitrary (fun (strong', value) ->
      let p = Fn.apply strong' in
      let (left, right) = test p in
      tuple_eq P.C.equal P.B.equal (left value) (right value) )
;;

let strong_contramap_fst (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair (fun1 P.A.observable P.B.arbitrary) (pair P.A.arbitrary P.C.arbitrary)
  in
  let (name, test) = Strong.contramap_fst in
  Test.make ~name ~count arbitrary (fun (strong', value) ->
      let p = Fn.apply strong' in
      let (left, right) = test p in
      P.B.equal (left value) (right value) )
;;

let strong_contramap_snd (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair (fun1 P.A.observable P.B.arbitrary) (pair P.C.arbitrary P.A.arbitrary)
  in
  let (name, test) = Strong.contramap_snd in
  Test.make ~name ~count arbitrary (fun (strong', value) ->
      let p = Fn.apply strong' in
      let (left, right) = test p in
      P.B.equal (left value) (right value) )
;;

let strong_dinaturality_fst (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    triple
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.D.arbitrary)
      (pair P.C.arbitrary P.A.arbitrary)
  in
  let (name, test) = Strong.dinaturality_fst in
  Test.make ~name ~count arbitrary (fun (f', strong', value) ->
      let f = Fn.apply f' in
      let p = Fn.apply strong' in
      let (left, right) = test f p in
      tuple_eq P.D.equal P.B.equal (left value) (right value) )
;;

let strong_dinaturality_snd (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    triple
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.D.arbitrary)
      (pair P.A.arbitrary P.C.arbitrary)
  in
  let (name, test) = Strong.dinaturality_snd in
  Test.make ~name ~count arbitrary (fun (f', strong', value) ->
      let f = Fn.apply f' in
      let p = Fn.apply strong' in
      let (left, right) = test f p in
      tuple_eq P.B.equal P.D.equal (left value) (right value) )
;;

let strong_fst_fst_is_dimap (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair
      (fun1 P.A.observable P.B.arbitrary)
      (pair (pair P.A.arbitrary P.C.arbitrary) P.D.arbitrary)
  in
  let (name, test) = Strong.fst_fst_is_dmap in
  Test.make ~name ~count arbitrary (fun (strong', value) ->
      let p = Fn.apply strong' in
      let (left, right) = test p in
      tuple_eq
        (tuple_eq P.B.equal P.C.equal)
        P.D.equal (left value) (right value) )
;;

let strong_snd_snd_is_dimap (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair
      (fun1
         (Observable.pair P.A.observable P.B.observable)
         (pair P.C.arbitrary P.D.arbitrary) )
      (pair P.E.arbitrary
         (pair P.F.arbitrary (pair P.A.arbitrary P.B.arbitrary)) )
  in
  let (name, test) = Strong.snd_snd_is_dmap in
  Test.make ~name ~count arbitrary (fun (strong', value) ->
      let p = Fn.apply strong' in
      let (left, right) = test p in
      tuple_eq P.E.equal
        (tuple_eq P.F.equal (tuple_eq P.C.equal P.D.equal))
        (left value) (right value) )
;;

let choice_dimap_id (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Choice.dimap_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
;;

let choice_fst_id (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Choice.contramap_fst_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
;;

let choice_snd_id (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Choice.map_snd_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
;;

let choice_dimap_eq (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    quad
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.D.arbitrary)
      (fun1 P.B.observable P.C.arbitrary)
      P.A.arbitrary
  in
  let (name, test) = Choice.dimap_equality in
  Test.make ~name ~count arbitrary (fun (f', g', choice', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let p = Fn.apply choice' in
      let (left, right) = test f g p in
      P.D.equal (left value) (right value) )
;;

let choice_dimap_param (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    quad
      (pair
         (fun1 P.A.observable P.B.arbitrary)
         (fun1 P.C.observable P.A.arbitrary) )
      (pair
         (fun1 P.D.observable P.E.arbitrary)
         (fun1 P.F.observable P.D.arbitrary) )
      (fun1 P.B.observable P.F.arbitrary)
      P.C.arbitrary
  in
  let (name, test) = Choice.dimap_parametricity in
  Test.make ~name ~count arbitrary (fun ((f', g'), (h', i'), choice', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let h = Fn.apply h' in
      let i = Fn.apply i' in
      let p = Fn.apply choice' in
      let (left, right) = test f g h i p in
      P.E.equal (left value) (right value) )
;;

let choice_fst_param (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    quad
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.A.arbitrary)
      (fun1 P.B.observable P.D.arbitrary)
      P.C.arbitrary
  in
  let (name, test) = Choice.contramap_fst_parametricity in
  Test.make ~name ~count arbitrary (fun (f', g', choice', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let p = Fn.apply choice' in
      let (left, right) = test f g p in
      P.D.equal (left value) (right value) )
;;

let choice_snd_param (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    quad
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.A.arbitrary)
      (fun1 P.D.observable P.C.arbitrary)
      P.D.arbitrary
  in
  let (name, test) = Choice.map_snd_parametricity in
  Test.make ~name ~count arbitrary (fun (f', g', choice', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let p = Fn.apply choice' in
      let (left, right) = test f g p in
      P.B.equal (left value) (right value) )
;;

let choice_left_defined_by_right (module P : Preface_qcheck.Sample.PACKAGE)
    count =
  let open QCheck in
  let arbitrary =
    pair
      (fun1 P.A.observable P.B.arbitrary)
      (either P.A.arbitrary P.C.arbitrary)
  in
  let (name, test) = Choice.left_defined_by_right in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      either_eq P.B.equal P.C.equal (left value) (right value) )
;;

let choice_right_defined_by_left (module P : Preface_qcheck.Sample.PACKAGE)
    count =
  let open QCheck in
  let arbitrary =
    pair
      (fun1 P.A.observable P.B.arbitrary)
      (either P.C.arbitrary P.A.arbitrary)
  in
  let (name, test) = Choice.right_defined_by_left in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      either_eq P.C.equal P.B.equal (left value) (right value) )
;;

let choice_map_snd_left (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Choice.map_snd_left in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      either_eq P.B.equal P.C.equal (left value) (right value) )
;;

let choice_map_snd_right (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Choice.map_snd_right in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      either_eq P.C.equal P.B.equal (left value) (right value) )
;;

let choice_contramp_fst_right (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    triple
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.D.arbitrary)
      (either P.C.arbitrary P.A.arbitrary)
  in
  let (name, test) = Choice.contramap_fst_right in
  Test.make ~name ~count arbitrary (fun (f', c', value) ->
      let f = Fn.apply f' in
      let c = Fn.apply c' in
      let (left, right) = test f c in
      either_eq P.D.equal P.B.equal (left value) (right value) )
;;

let choice_contramp_fst_left (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    triple
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.D.arbitrary)
      (either P.A.arbitrary P.C.arbitrary)
  in
  let (name, test) = Choice.contramap_fst_left in
  Test.make ~name ~count arbitrary (fun (f', c', value) ->
      let f = Fn.apply f' in
      let c = Fn.apply c' in
      let (left, right) = test f c in
      either_eq P.B.equal P.D.equal (left value) (right value) )
;;

let choice_left_left (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair
      (fun1 P.A.observable P.B.arbitrary)
      (either (either P.A.arbitrary P.C.arbitrary) P.D.arbitrary)
  in
  let (name, test) = Choice.left_left in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      either_eq
        (either_eq P.B.equal P.C.equal)
        P.D.equal (left value) (right value) )
;;

let choice_right_right (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair
      (fun1 P.A.observable P.B.arbitrary)
      (either P.C.arbitrary (either P.D.arbitrary P.A.arbitrary))
  in
  let (name, test) = Choice.right_right in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      either_eq P.C.equal
        (either_eq P.D.equal P.B.equal)
        (left value) (right value) )
;;

let closed_dimap_id (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Closed.dimap_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
;;

let closed_fst_id (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Closed.contramap_fst_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
;;

let closed_snd_id (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Closed.map_snd_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
;;

let closed_dimap_eq (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    quad
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.D.arbitrary)
      (fun1 P.B.observable P.C.arbitrary)
      P.A.arbitrary
  in
  let (name, test) = Closed.dimap_equality in
  Test.make ~name ~count arbitrary (fun (f', g', pro', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let p = Fn.apply pro' in
      let (left, right) = test f g p in
      P.D.equal (left value) (right value) )
;;

let closed_dimap_param (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    quad
      (pair
         (fun1 P.A.observable P.B.arbitrary)
         (fun1 P.C.observable P.A.arbitrary) )
      (pair
         (fun1 P.D.observable P.E.arbitrary)
         (fun1 P.F.observable P.D.arbitrary) )
      (fun1 P.B.observable P.F.arbitrary)
      P.C.arbitrary
  in
  let (name, test) = Closed.dimap_parametricity in
  Test.make ~name ~count arbitrary (fun ((f', g'), (h', i'), pro', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let h = Fn.apply h' in
      let i = Fn.apply i' in
      let p = Fn.apply pro' in
      let (left, right) = test f g h i p in
      P.E.equal (left value) (right value) )
;;

let closed_fst_param (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    quad
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.A.arbitrary)
      (fun1 P.B.observable P.D.arbitrary)
      P.C.arbitrary
  in
  let (name, test) = Closed.contramap_fst_parametricity in
  Test.make ~name ~count arbitrary (fun (f', g', pro', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let p = Fn.apply pro' in
      let (left, right) = test f g p in
      P.D.equal (left value) (right value) )
;;

let closed_snd_param (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    quad
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.A.arbitrary)
      (fun1 P.D.observable P.C.arbitrary)
      P.D.arbitrary
  in
  let (name, test) = Closed.map_snd_parametricity in
  Test.make ~name ~count arbitrary (fun (f', g', pro', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let p = Fn.apply pro' in
      let (left, right) = test f g p in
      P.B.equal (left value) (right value) )
;;

let closed_contramap_fst_closed (module P : Preface_qcheck.Sample.PACKAGE) count
    =
  let open QCheck in
  let arbitrary =
    quad
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.D.arbitrary)
      (fun1 P.B.observable P.C.arbitrary)
      P.A.arbitrary
  in
  let (name, test) = Closed.contramap_fst_closed in
  Test.make ~name ~count arbitrary (fun (f', pro', g', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let p = Fn.apply pro' in
      let (left, right) = test f p in
      P.D.equal (left g value) (right g value) )
;;

let closed_closed (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    triple
      (fun1 P.A.observable P.B.arbitrary)
      (fun2 P.C.observable P.D.observable P.A.arbitrary)
      (pair P.C.arbitrary P.D.arbitrary)
  in

  let (name, test) = Closed.closed_closed in
  Test.make ~name ~count arbitrary (fun (p', f', (a, b)) ->
      let f = Fn.apply f' in
      let p = Fn.apply p' in
      let (left, right) = test p in
      P.B.equal (left f a b) (right f a b) )
;;

let closed_dimap_const (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in

  let (name, test) = Closed.dimap_const in
  Test.make ~name ~count arbitrary (fun (p', value) ->
      let p = Fn.apply p' in
      let (left, right) = test p in
      P.B.equal (left value) (right value) )
;;

let cat_right_identity (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Category.right_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
;;

let cat_left_identity (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Category.left_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
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
      P.B.equal (left value) (right value) )
;;

let arrow_right_identity (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Arrow.right_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
;;

let arrow_left_identity (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Arrow.left_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
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
      P.B.equal (left value) (right value) )
;;

let arrow_law1 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = P.A.arbitrary in
  let (name, test) = Arrow.law1 in
  Test.make ~name ~count arbitrary (fun value ->
      let (left, right) = test () in
      P.A.equal (left value) (right value) )
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
      P.C.equal (left value) (right value) )
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
      tuple_eq P.B.equal P.C.equal (left value) (right value) )
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
      tuple_eq P.C.equal P.D.equal (left value) (right value) )
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
      tuple_eq P.B.equal P.D.equal (left value) (right value) )
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
      P.B.equal (left value) (right value) )
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
        (left value) (right value) )
;;

let arrow_choice_right_identity (module P : Preface_qcheck.Sample.PACKAGE) count
    =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Arrow_choice.right_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
;;

let arrow_choice_left_identity (module P : Preface_qcheck.Sample.PACKAGE) count
    =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Arrow_choice.left_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
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
  let (name, test) = Arrow_choice.associativity in
  Test.make ~name ~count arbitrary (fun (f', g', h', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let h = Fn.apply h' in
      let (left, right) = test f g h in
      P.B.equal (left value) (right value) )
;;

let arrow_choice_law1 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = P.A.arbitrary in
  let (name, test) = Arrow_choice.law1 in
  Test.make ~name ~count arbitrary (fun value ->
      let (left, right) = test () in
      P.A.equal (left value) (right value) )
;;

let arrow_choice_law2 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    triple
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.B.observable P.C.arbitrary)
      P.A.arbitrary
  in
  let (name, test) = Arrow_choice.law2 in
  Test.make ~name ~count arbitrary (fun (f', g', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let (left, right) = test f g in
      P.C.equal (left value) (right value) )
;;

let arrow_choice_law3 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair (fun1 P.A.observable P.B.arbitrary) (pair P.A.arbitrary P.C.arbitrary)
  in
  let (name, test) = Arrow_choice.law3 in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      tuple_eq P.B.equal P.C.equal (left value) (right value) )
;;

let arrow_choice_law4 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    triple
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.B.observable P.C.arbitrary)
      (pair P.A.arbitrary P.D.arbitrary)
  in
  let (name, test) = Arrow_choice.law4 in
  Test.make ~name ~count arbitrary (fun (f', g', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let (left, right) = test f g in
      tuple_eq P.C.equal P.D.equal (left value) (right value) )
;;

let arrow_choice_law5 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    triple
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.D.arbitrary)
      (pair P.A.arbitrary P.C.arbitrary)
  in
  let (name, test) = Arrow_choice.law5 in
  Test.make ~name ~count arbitrary (fun (f', g', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let (left, right) = test f g in
      tuple_eq P.B.equal P.D.equal (left value) (right value) )
;;

let arrow_choice_law6 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair (fun1 P.A.observable P.B.arbitrary) (pair P.A.arbitrary P.C.arbitrary)
  in
  let (name, test) = Arrow_choice.law6 in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
;;

let arrow_choice_law7 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair
      (fun1 P.A.observable P.B.arbitrary)
      (pair (pair P.A.arbitrary P.C.arbitrary) P.D.arbitrary)
  in
  let (name, test) = Arrow_choice.law7 in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Preface_stdlib.Fun.Arrow_choice.arrow (Fn.apply f') in
      let (left, right) = test f in
      tuple_eq P.B.equal
        (tuple_eq P.C.equal P.D.equal)
        (left value) (right value) )
;;

let arrow_choice_law8 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair
      (fun1 P.A.observable P.B.arbitrary)
      (either P.A.arbitrary P.C.arbitrary)
  in
  let (name, test) = Arrow_choice.law8 in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      either_eq P.B.equal P.C.equal (left value) (right value) )
;;

let arrow_choice_law9 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    triple
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.B.observable P.C.arbitrary)
      (either P.A.arbitrary P.D.arbitrary)
  in
  let (name, test) = Arrow_choice.law9 in
  Test.make ~name ~count arbitrary (fun (f', g', value) ->
      let f = Preface_stdlib.Fun.Arrow_choice.arrow (Fn.apply f') in
      let g = Preface_stdlib.Fun.Arrow_choice.arrow (Fn.apply g') in
      let (left, right) = test f g in
      either_eq P.C.equal P.D.equal (left value) (right value) )
;;

let arrow_choice_law10 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Arrow_choice.law10 in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Preface_stdlib.Fun.Arrow_choice.arrow (Fn.apply f') in
      let (left, right) = test f in
      either_eq P.B.equal P.C.equal (left value) (right value) )
;;

let arrow_choice_law11 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    triple
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.D.arbitrary)
      (either P.A.arbitrary P.C.arbitrary)
  in
  let (name, test) = Arrow_choice.law11 in
  Test.make ~name ~count arbitrary (fun (f', g', value) ->
      let f = Preface_stdlib.Fun.Arrow_choice.arrow (Fn.apply f') in
      let g = Fn.apply g' in
      let (left, right) = test f g in
      either_eq P.B.equal P.D.equal (left value) (right value) )
;;

let arrow_choice_law12 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair
      (fun1 P.A.observable P.B.arbitrary)
      (either (either P.A.arbitrary P.C.arbitrary) P.D.arbitrary)
  in
  let (name, test) = Arrow_choice.law12 in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Preface_stdlib.Fun.Arrow_choice.arrow (Fn.apply f') in
      let (left, right) = test f in
      either_eq P.B.equal
        (either_eq P.C.equal P.D.equal)
        (left value) (right value) )
;;

let arrow_apply_right_identity (module P : Preface_qcheck.Sample.PACKAGE) count
    =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Arrow_apply.right_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
;;

let arrow_apply_left_identity (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair (fun1 P.A.observable P.B.arbitrary) P.A.arbitrary in
  let (name, test) = Arrow_apply.left_identity in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
;;

let arrow_apply_associativity (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    quad
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.A.arbitrary)
      (fun1 P.D.observable P.C.arbitrary)
      P.D.arbitrary
  in
  let (name, test) = Arrow_apply.associativity in
  Test.make ~name ~count arbitrary (fun (f', g', h', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let h = Fn.apply h' in
      let (left, right) = test f g h in
      P.B.equal (left value) (right value) )
;;

let arrow_apply_law1 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = P.A.arbitrary in
  let (name, test) = Arrow_apply.law1 in
  Test.make ~name ~count arbitrary (fun value ->
      let (left, right) = test () in
      P.A.equal (left value) (right value) )
;;

let arrow_apply_law2 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    triple
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.B.observable P.C.arbitrary)
      P.A.arbitrary
  in
  let (name, test) = Arrow_apply.law2 in
  Test.make ~name ~count arbitrary (fun (f', g', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let (left, right) = test f g in
      P.C.equal (left value) (right value) )
;;

let arrow_apply_law3 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair (fun1 P.A.observable P.B.arbitrary) (pair P.A.arbitrary P.C.arbitrary)
  in
  let (name, test) = Arrow_apply.law3 in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      tuple_eq P.B.equal P.C.equal (left value) (right value) )
;;

let arrow_apply_law4 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    triple
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.B.observable P.C.arbitrary)
      (pair P.A.arbitrary P.D.arbitrary)
  in
  let (name, test) = Arrow_apply.law4 in
  Test.make ~name ~count arbitrary (fun (f', g', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let (left, right) = test f g in
      tuple_eq P.C.equal P.D.equal (left value) (right value) )
;;

let arrow_apply_law5 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    triple
      (fun1 P.A.observable P.B.arbitrary)
      (fun1 P.C.observable P.D.arbitrary)
      (pair P.A.arbitrary P.C.arbitrary)
  in
  let (name, test) = Arrow_apply.law5 in
  Test.make ~name ~count arbitrary (fun (f', g', value) ->
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let (left, right) = test f g in
      tuple_eq P.B.equal P.D.equal (left value) (right value) )
;;

let arrow_apply_law6 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair (fun1 P.A.observable P.B.arbitrary) (pair P.A.arbitrary P.C.arbitrary)
  in
  let (name, test) = Arrow_apply.law6 in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Fn.apply f' in
      let (left, right) = test f in
      P.B.equal (left value) (right value) )
;;

let arrow_apply_law7 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair
      (fun1 P.A.observable P.B.arbitrary)
      (pair (pair P.A.arbitrary P.C.arbitrary) P.D.arbitrary)
  in
  let (name, test) = Arrow_apply.law7 in
  Test.make ~name ~count arbitrary (fun (f', value) ->
      let f = Preface_stdlib.Fun.Arrow_apply.arrow (Fn.apply f') in
      let (left, right) = test f in
      tuple_eq P.B.equal
        (tuple_eq P.C.equal P.D.equal)
        (left value) (right value) )
;;

let arrow_apply_law8 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary = pair P.A.arbitrary P.B.arbitrary in
  let (name, test) = Arrow_apply.law8 in
  Test.make ~name ~count arbitrary (fun value ->
      let (left, right) = test () in
      tuple_eq P.A.equal P.B.equal (left value) (right value) )
;;

let arrow_apply_law9 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair
      (fun1 P.A.observable P.B.arbitrary)
      (pair (fun1 P.B.observable P.C.arbitrary) P.A.arbitrary)
  in
  let (name, test) = Arrow_apply.law9 in
  Test.make ~name ~count arbitrary (fun (f', (g', value)) ->
      let f = Preface_stdlib.Fun.Arrow_apply.arrow (Fn.apply f') in
      let g = Preface_stdlib.Fun.Arrow_apply.arrow (Fn.apply g') in
      let (left, right) = test f in
      let l = left (g, value) in
      let r = right (g, value) in
      P.C.equal l r )
;;

let arrow_apply_law10 (module P : Preface_qcheck.Sample.PACKAGE) count =
  let open QCheck in
  let arbitrary =
    pair
      (fun1 P.A.observable P.B.arbitrary)
      (pair (fun1 P.C.observable P.A.arbitrary) P.C.arbitrary)
  in
  let (name, test) = Arrow_apply.law10 in
  Test.make ~name ~count arbitrary (fun (f', (g', value)) ->
      let f = Preface_stdlib.Fun.Arrow_apply.arrow (Fn.apply f') in
      let g = Preface_stdlib.Fun.Arrow_apply.arrow (Fn.apply g') in
      let (left, right) = test f in
      let l = left (g, value) in
      let r = right (g, value) in
      P.B.equal l r )
;;

module Req = struct
  type 'a t = (unit, 'a) Preface_stdlib.Fun.t

  let arbitrary x = QCheck.(fun1 Observable.unit x |> map Fn.apply)

  let observable x =
    let open QCheck.Observable in
    let eq l r = (equal x) (l ()) (r ()) in
    let hash l = (hash x) (l ()) in
    let print f = (print x) (f ()) in
    QCheck.Observable.make ~eq ~hash print
  ;;

  let equal eq x y = eq (x ()) (y ())
end

module Functor =
  Preface_laws.Functor.Cases
    (Preface_make.Functor.From_arrow (Preface_stdlib.Fun.Arrow)) (Req)
    (Preface_qcheck.Sample.Pack1)
module Applicative =
  Preface_laws.Applicative.Cases
    (Preface_make.Applicative.From_arrow (Preface_stdlib.Fun.Arrow)) (Req)
    (Preface_qcheck.Sample.Pack1)
module Monad =
  Preface_laws.Monad.Cases
    (Preface_make.Monad.From_arrow_apply (Preface_stdlib.Fun.Arrow_apply)) (Req)
    (Preface_qcheck.Sample.Pack1)
module Selective =
  Preface_laws.Selective.Cases
    (Preface_make.Selective.From_arrow_choice
       (Preface_stdlib.Fun.Arrow_choice))
       (Req)
    (Preface_qcheck.Sample.Pack1)

let cases n =
  [
    ( "Fun Profunctor Laws"
    , [
        pro_dimap_id (module Preface_qcheck.Sample.Pack1) n
      ; pro_fst_id (module Preface_qcheck.Sample.Pack1) n
      ; pro_snd_id (module Preface_qcheck.Sample.Pack1) n
      ; pro_dimap_eq (module Preface_qcheck.Sample.Pack1) n
      ; pro_dimap_param (module Preface_qcheck.Sample.Pack1) n
      ; pro_fst_param (module Preface_qcheck.Sample.Pack1) n
      ; pro_snd_param (module Preface_qcheck.Sample.Pack1) n
      ]
      |> Stdlib.List.map QCheck_alcotest.to_alcotest )
  ; ( "Fun Strong Laws"
    , [
        strong_dimap_id (module Preface_qcheck.Sample.Pack1) n
      ; strong_fst_id (module Preface_qcheck.Sample.Pack1) n
      ; strong_snd_id (module Preface_qcheck.Sample.Pack1) n
      ; strong_dimap_eq (module Preface_qcheck.Sample.Pack1) n
      ; strong_dimap_param (module Preface_qcheck.Sample.Pack1) n
      ; strong_fst_param (module Preface_qcheck.Sample.Pack1) n
      ; strong_snd_param (module Preface_qcheck.Sample.Pack1) n
      ; strong_fst_define_snd (module Preface_qcheck.Sample.Pack1) n
      ; strong_snd_define_fst (module Preface_qcheck.Sample.Pack1) n
      ; strong_contramap_fst (module Preface_qcheck.Sample.Pack1) n
      ; strong_contramap_snd (module Preface_qcheck.Sample.Pack1) n
      ; strong_dinaturality_fst (module Preface_qcheck.Sample.Pack1) n
      ; strong_dinaturality_snd (module Preface_qcheck.Sample.Pack1) n
      ; strong_fst_fst_is_dimap (module Preface_qcheck.Sample.Pack1) n
      ; strong_snd_snd_is_dimap (module Preface_qcheck.Sample.Pack1) n
      ]
      |> Stdlib.List.map QCheck_alcotest.to_alcotest )
  ; ( "Fun Choice Laws"
    , [
        choice_dimap_id (module Preface_qcheck.Sample.Pack1) n
      ; choice_fst_id (module Preface_qcheck.Sample.Pack1) n
      ; choice_snd_id (module Preface_qcheck.Sample.Pack1) n
      ; choice_dimap_eq (module Preface_qcheck.Sample.Pack1) n
      ; choice_dimap_param (module Preface_qcheck.Sample.Pack1) n
      ; choice_fst_param (module Preface_qcheck.Sample.Pack1) n
      ; choice_snd_param (module Preface_qcheck.Sample.Pack1) n
      ; choice_left_defined_by_right (module Preface_qcheck.Sample.Pack1) n
      ; choice_right_defined_by_left (module Preface_qcheck.Sample.Pack1) n
      ; choice_map_snd_left (module Preface_qcheck.Sample.Pack1) n
      ; choice_map_snd_right (module Preface_qcheck.Sample.Pack1) n
      ; choice_contramp_fst_right (module Preface_qcheck.Sample.Pack1) n
      ; choice_contramp_fst_left (module Preface_qcheck.Sample.Pack1) n
      ; choice_left_left (module Preface_qcheck.Sample.Pack1) n
      ; choice_right_right (module Preface_qcheck.Sample.Pack1) n
      ]
      |> Stdlib.List.map QCheck_alcotest.to_alcotest )
  ; ( "Fun Closed Laws"
    , [
        closed_dimap_id (module Preface_qcheck.Sample.Pack1) n
      ; closed_fst_id (module Preface_qcheck.Sample.Pack1) n
      ; closed_snd_id (module Preface_qcheck.Sample.Pack1) n
      ; closed_dimap_eq (module Preface_qcheck.Sample.Pack1) n
      ; closed_dimap_param (module Preface_qcheck.Sample.Pack1) n
      ; closed_fst_param (module Preface_qcheck.Sample.Pack1) n
      ; closed_snd_param (module Preface_qcheck.Sample.Pack1) n
      ; closed_contramap_fst_closed (module Preface_qcheck.Sample.Pack1) n
      ; closed_closed (module Preface_qcheck.Sample.Pack1) n
      ; closed_dimap_const (module Preface_qcheck.Sample.Pack1) n
      ]
      |> Stdlib.List.map QCheck_alcotest.to_alcotest )
  ; ( "Fun Category Laws"
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
  ; ( "Fun Arrow Apply Laws"
    , [
        arrow_apply_right_identity (module Preface_qcheck.Sample.Pack1) n
      ; arrow_apply_left_identity (module Preface_qcheck.Sample.Pack1) n
      ; arrow_apply_associativity (module Preface_qcheck.Sample.Pack1) n
      ; arrow_apply_law1 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_apply_law2 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_apply_law3 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_apply_law4 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_apply_law5 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_apply_law6 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_apply_law7 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_apply_law8 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_apply_law9 (module Preface_qcheck.Sample.Pack1) n
      ; arrow_apply_law10 (module Preface_qcheck.Sample.Pack1) n
      ]
      |> Stdlib.List.map QCheck_alcotest.to_alcotest )
  ; ("Fun Functor Laws (using Arrow Monad)", Functor.cases n)
  ; ("Fun Applicative Laws (using Arrow Monad)", Applicative.cases n)
  ; ("Fun Monad Laws (using Arrow Monad)", Monad.cases n)
  ; ("Fun Selective Laws (using Arrow Monad)", Selective.cases n)
  ]
;;
