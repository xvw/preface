module Make
    (B : Preface_specs.BIFUNCTOR)
    (R : Requirement.INPUT_T2 with type ('a, 'b) t = ('a, 'b) B.t)
    (P : Sample.PACK) : Requirement.OUTPUT = struct
  open QCheck

  open Helper.Make_for_t2 (R) (P)

  module Underlying = Preface_make.Bifunctor.Via_bimap (B)

  let l = t1

  let l' = t1'

  let r = t2

  let r' = t2'

  (* Test bifunctor laws *)

  let identity =
    let test_name = "bimap id id = id"
    and test_arbitrary = over l r
    and test x =
      let left = B.bimap (fun x -> x) (fun x -> x) x
      and right = x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let first =
    let test_name = "fst id = id"
    and test_arbitrary = over l r
    and test x =
      let left = B.fst (fun x -> x) x
      and right = x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let second =
    let test_name = "snd id = id"
    and test_arbitrary = over l r
    and test x =
      let open B in
      let left = snd (fun x -> x) x
      and right = x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let bimap_fst_snd =
    let test_name = "bimap f g = (fst f) % (snd g)"
    and test_arbitrary = triple (over l r) (fun1 l' t3) (fun1 r' t4)
    and test (x, f', g') =
      let open B in
      let open Preface_core.Fun in
      let f = QCheck.Fn.apply f' in
      let g = QCheck.Fn.apply g' in
      let left = bimap f g x
      and right = (fst f % snd g) x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let bimap_parametricity =
    let test_name = "bimap (f % g) (h % i) = (bimap f h) % (bimap g i)"
    and test_arbitrary =
      triple (over l r)
        (pair (fun1 t3' t4) (fun1 l' t3))
        (pair (fun1 t5' t6) (fun1 r' t5))
    and test (x, (f', g'), (h', i')) =
      let open B in
      let open Preface_core.Fun in
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let h = Fn.apply h' in
      let i = Fn.apply i' in
      let left = (bimap (f % g) (h % i)) x
      and right = (bimap f h % bimap g i) x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let fst_parametricity =
    let test_name = "fst (f % g) = (fst f) % (fst g)"
    and test_arbitrary = triple (over l r) (fun1 t3' t4) (fun1 l' t3)
    and test (x, f', g') =
      let open B in
      let open Preface_core.Fun in
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let left = fst (f % g) x
      and right = (fst f % fst g) x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let snd_parametricity =
    let test_name = "snd (f % g) = (snd f) % (snd g)"
    and test_arbitrary = triple (over l r) (fun1 t3' t4) (fun1 r' t3)
    and test (x, f', g') =
      let open B in
      let open Preface_core.Fun in
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let left = snd (f % g) x
      and right = (snd f % snd g) x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let fst =
    let test_name = "fst"
    and test_arbitrary = pair (over l r) (fun1 l' t3)
    and test (x, f') =
      let f = Fn.apply f' in
      let left = B.fst f x
      and right = Underlying.fst f x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let snd =
    let test_name = "snd"
    and test_arbitrary = pair (over l r) (fun1 r' t3)
    and test (x, f') =
      let f = Fn.apply f' in
      let left = B.snd f x
      and right = Underlying.snd f x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let replace_fst =
    let test_name = "replace_fst"
    and test_arbitrary = pair (over l r) t3
    and test (x, value) =
      let left = B.replace_fst value x
      and right = Underlying.replace_fst value x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let replace_snd =
    let test_name = "replace_snd"
    and test_arbitrary = pair (over l r) t3
    and test (x, value) =
      let left = B.replace_snd value x
      and right = Underlying.replace_snd value x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let cases =
    [
      ( "Bifunctor " ^ R.name ^ " laws"
      , [
          identity
        ; first
        ; second
        ; bimap_fst_snd
        ; bimap_parametricity
        ; fst_parametricity
        ; snd_parametricity
        ]
        |> List.map QCheck_alcotest.to_alcotest )
    ; ( "Bifunctor " ^ R.name ^ " has expected behaviour"
      , [ fst; snd; replace_fst; replace_snd ]
        |> List.map QCheck_alcotest.to_alcotest )
    ]
  ;;
end
