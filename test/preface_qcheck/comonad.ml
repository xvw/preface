module Make_hooked
    (C : Preface_specs.COMONAD)
    (R : Requirement.INPUT_T1 with type 'a t = 'a C.t)
    (Hook : Requirement.HOOK with type 'a t = 'a C.t)
    (Obs : sig
      type 'a t

      val f : 'a QCheck.Observable.t -> 'a t QCheck.Observable.t
    end
    with type 'a t = 'a C.t)
    (P : Sample.PACK) : Requirement.OUTPUT = struct
  open QCheck

  open Helper.Make_for_t1 (R) (P)

  module Underlying = Preface_make.Comonad.Via_extend (C)

  (* Test Comonad laws *)

  let preserve_identity =
    let test_name = "extend extract = id"
    and test_arbitrary = over t1
    and test x =
      let open C in
      let open Preface_stdlib.Fun in
      let left = extend extract x
      and right = id x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let extract_extend =
    let test_name = "extract % extend f = f"
    and test_arbitrary = pair (fun1 (Obs.f t1') t2) (over t1)
    and test (f', x) =
      let open C in
      let open Preface_stdlib.Fun in
      let f = Fn.apply f' in
      let left = (extract % extend f) x
      and right = f x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let extend_extend =
    let test_name = "extend f % extend g = extend (f % extend g)"
    and test_arbitrary =
      triple (fun1 (Obs.f t3') t4) (fun1 (Obs.f t1') t3) (over t1)
    and test (f', g', x) =
      let open C in
      let open Preface_stdlib.Fun in
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let left = (extend f % extend g) x
      and right = (extend (f % extend g)) x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let infix_extract_extend =
    let test_name = "f =>= extract = f"
    and test_arbitrary = pair (fun1 (Obs.f t1') t2) (over t1)
    and test (f', x) =
      let open C in
      let f = Fn.apply f' in
      let left = (f =>= extract) x
      and right = f x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let rev_infix_extract_extend =
    let test_name = "extract =>= f = f"
    and test_arbitrary = pair (fun1 (Obs.f t1') t2) (over t1)
    and test (f', x) =
      let open C in
      let f = Fn.apply f' in
      let left = (extract =>= f) x
      and right = f x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let infix_extend =
    let test_name = "(f =>= g) =>= h = f =>= (g =>= h)"
    and test_arbitrary =
      quad
        (fun1 (Obs.f t1') t2)
        (fun1 (Obs.f t2') t3)
        (fun1 (Obs.f t3') t4)
        (over t1)
    and test (f', g', h', x) =
      let open C in
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let h = Fn.apply h' in
      let left = (f =>= g =>= h) x
      and right = (f =>= (g =>= h)) x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let duplicate_preserve_identity =
    let test_name = "extract % duplicate = id"
    and test_arbitrary = over t1
    and test x =
      let open C in
      let open Preface_stdlib.Fun in
      let left = (extract % duplicate) x
      and right = id x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let map_duplicate_preserve_identity =
    let test_name = "map extract % duplicate = id"
    and test_arbitrary = over t1
    and test x =
      let open C in
      let open Preface_stdlib.Fun in
      let left = (map extract % duplicate) x
      and right = id x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let map_duplicate_duplicate =
    let test_name = "duplicate % duplicate = map duplicate % duplicate"
    and test_arbitrary = over t1
    and test x =
      let open C in
      let open Preface_stdlib.Fun in
      let left =
        (duplicate % duplicate) x
        |> C.map (fun x -> Hook.apply (C.map Hook.apply x))
      and right =
        (map duplicate % duplicate) x
        |> C.map (fun x -> Hook.apply (C.map Hook.apply x))
      in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let extend_map_duplicate =
    let test_name = "extend f = map f % duplicate"
    and test_arbitrary = pair (fun1 (Obs.f t1') t2) (over t1)
    and test (f', x) =
      let open C in
      let open Preface_stdlib.Fun in
      let f = Fn.apply f' in
      let left = (extend f) x
      and right = (map f % duplicate) x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let duplicate_extract_id =
    let test_name = "duplicate = extend id"
    and test_arbitrary = over t1
    and test x =
      let open C in
      let left = duplicate x |> C.map Hook.apply
      and right = (extend Fun.id) x |> C.map Hook.apply in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let map_extend_extract =
    let test_name = "map f = extend (f % extract)"
    and test_arbitrary = pair (fun1 t1' t2) (over t1)
    and test (f', x) =
      let open C in
      let open Preface_stdlib.Fun in
      let f = Fn.apply f' in
      let left = map f x
      and right = (extend (f % extract)) x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let cases =
    [
      ( "Comonad " ^ R.name ^ " laws"
      , [
          preserve_identity
        ; extract_extend
        ; extend_extend
        ; infix_extract_extend
        ; rev_infix_extract_extend
        ; infix_extend
        ; duplicate_preserve_identity
        ; map_duplicate_preserve_identity
        ; map_duplicate_duplicate
        ; extend_map_duplicate
        ; duplicate_extract_id
        ; map_extend_extract
        ]
        |> List.map QCheck_alcotest.to_alcotest )
    ; ( "Comonad " ^ R.name ^ " has expected behaviour"
      , [] |> List.map QCheck_alcotest.to_alcotest )
    ]
  ;;
end
