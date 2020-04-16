module type BIFUNCTOR = Preface_specs.BIFUNCTOR

module Make
    (Bifunctor : BIFUNCTOR)
    (Req : sig
      type ('a, 'b) t

      val suite_name : string

      val arbitrary :
           'a QCheck.arbitrary
        -> 'b QCheck.arbitrary
        -> ('a, 'b) t QCheck.arbitrary
    end
    with type ('a, 'b) t = ('a, 'b) Bifunctor.t)
    (L : Qcheck_helpers.GENERATOR)
    (R : Qcheck_helpers.GENERATOR)
    (F : Qcheck_helpers.GENERATOR)
    (G : Qcheck_helpers.GENERATOR)
    (H : Qcheck_helpers.GENERATOR)
    (I : Qcheck_helpers.GENERATOR) : Qcheck_helpers.ALCOTEST_SUITE = struct
  open QCheck

  (* Combinators for QCheck generator *)
  let bifunctor_of = Req.arbitrary

  let l = L.arbitrary

  let r = R.arbitrary

  let f = F.arbitrary

  let g = G.arbitrary

  let h = H.arbitrary

  let i = I.arbitrary

  let l_input = L.observable

  let r_input = R.observable

  let f_input = F.observable

  let h_input = H.observable

  let identity =
    let test_name = "bimap id id = id"
    and test_arbitrary = bifunctor_of l r
    and test x =
      let open Bifunctor in
      let left = bimap (fun x -> x) (fun x -> x) x
      and right = x in
      left = right
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let first =
    let test_name = "fst id = id"
    and test_arbitrary = bifunctor_of l r
    and test x =
      let open Bifunctor in
      let left = fst (fun x -> x) x
      and right = x in
      left = right
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let second =
    let test_name = "snd id = id"
    and test_arbitrary = bifunctor_of l r
    and test x =
      let open Bifunctor in
      let left = snd (fun x -> x) x
      and right = x in
      left = right
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let bimap_fst_snd =
    let test_name = "bimap f g = (fst f) % (snd g)"
    and test_arbitrary =
      triple (bifunctor_of l r) (fun1 l_input f) (fun1 r_input g)
    and test (x, f', g') =
      let open Bifunctor in
      let open Preface_core.Fun in
      let f = QCheck.Fn.apply f' in
      let g = QCheck.Fn.apply g' in
      let left = bimap f g x
      and right = (fst f % snd g) x in
      left = right
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let bimap_parametricity =
    let test_name = "bimap (f % g) (h % i) = (bimap f h) % (bimap g i)"
    and test_arbitrary =
      triple (bifunctor_of l r)
        (pair (fun1 f_input g) (fun1 l_input f))
        (pair (fun1 h_input i) (fun1 r_input h))
    and test (x, (f', g'), (h', i')) =
      let open Bifunctor in
      let open Preface_core.Fun in
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let h = Fn.apply h' in
      let i = Fn.apply i' in
      let left = (bimap (f % g) (h % i)) x
      and right = (bimap f h % bimap g i) x in
      left = right
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let fst_parametricity =
    let test_name = "fst (f % g) = (fst f) % (fst g)"
    and test_arbitrary =
      triple (bifunctor_of l r) (fun1 f_input g) (fun1 l_input f)
    and test (x, f', g') =
      let open Bifunctor in
      let open Preface_core.Fun in
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let left = fst (f % g) x
      and right = (fst f % fst g) x in
      left = right
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let snd_parametricity =
    let test_name = "snd (f % g) = (snd f) % (snd g)"
    and test_arbitrary =
      triple (bifunctor_of l r) (fun1 f_input g) (fun1 r_input f)
    and test (x, f', g') =
      let open Bifunctor in
      let open Preface_core.Fun in
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let left = snd (f % g) x
      and right = (snd f % snd g) x in
      left = right
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let bimap_fst_id =
    let test_name = "fst f = bimap f id"
    and test_arbitrary = pair (bifunctor_of l r) (fun1 l_input f)
    and test (x, f') =
      let open Bifunctor in
      let open Preface_core.Fun in
      let f = Fn.apply f' in
      let left = bimap f id x
      and right = fst f x in
      left = right
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let bimap_snd_id =
    let test_name = "snd f = bimap id f"
    and test_arbitrary = pair (bifunctor_of l r) (fun1 r_input f)
    and test (x, f') =
      let open Bifunctor in
      let open Preface_core.Fun in
      let f = Fn.apply f' in
      let left = bimap id f x
      and right = snd f x in
      left = right
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let replace_fst =
    let test_name = "replace = map % const"
    and test_arbitrary = pair (bifunctor_of l r) g
    and test (x, value) =
      let open Bifunctor in
      let open Preface_core.Fun in
      let left = replace_fst value x
      and right = fst (const value) x in
      left = right
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let replace_snd =
    let test_name = "replace = map % const"
    and test_arbitrary = pair (bifunctor_of l r) g
    and test (x, value) =
      let open Bifunctor in
      let open Preface_core.Fun in
      let left = replace_snd value x
      and right = snd (const value) x in
      left = right
    in
    Test.make ~name:test_name ~count:100 test_arbitrary test
  ;;

  let cases =
    ( Req.suite_name ^ " Bifunctor"
    , List.map QCheck_alcotest.to_alcotest
        [
          identity
        ; first
        ; second
        ; bimap_fst_snd
        ; bimap_parametricity
        ; fst_parametricity
        ; snd_parametricity
        ; bimap_fst_id
        ; bimap_snd_id
        ; replace_fst
        ; replace_snd
        ] )
  ;;
end
